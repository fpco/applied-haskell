# Performance

Section exercise: write a Gauge benchmark suite to test various
implementations of `average` against each other.

* Profiling
* Optimization
* Analyzing core
* A few common problems

## Some bad code

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

main :: IO ()
main =
  let nums = [1..10000000 :: Int]
   in print $ fromIntegral (sum nums) / fromIntegral (length nums)
```

Run it, it's slow:

```
$ time ./Main.hs
5000000.5

real	0m5.478s
user	0m4.048s
sys	0m1.114s
```

First question: can anyone spot the problem? That's the easiest
solution to a problem... (If you do know, don't say anything yet.)

OK, assuming no one knows why this is so slow (or we can pretend we
don't know)...

### Basic statistics

Run it with:

```
$ stack exec -- ghc Main.hs -rtsopts -fforce-recomp && ./Main +RTS -s
5000000.5
   2,742,160,608 bytes allocated in the heap
   5,360,152,448 bytes copied during GC
   1,551,144,024 bytes maximum residency (14 sample(s))
     137,354,776 bytes maximum slop
            2954 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      5258 colls,     0 par    1.547s   1.586s     0.0003s    0.0018s
  Gen  1        14 colls,     0 par    2.835s   5.626s     0.4019s    2.0202s

  INIT    time    0.000s  (  0.002s elapsed)
  MUT     time    0.781s  (  0.602s elapsed)
  GC      time    4.382s  (  7.212s elapsed)
  RP      time    0.000s  (  0.000s elapsed)
  PROF    time    0.000s  (  0.000s elapsed)
  EXIT    time    0.012s  (  0.278s elapsed)
  Total   time    5.176s  (  8.094s elapsed)

  %GC     time      84.7%  (89.1% elapsed)

  Alloc rate    3,511,417,338 bytes per MUT second

  Productivity  15.3% of total user, 9.8% of total elapsed
```

* Max residency: largest amount of memory used at any time
* Bytes allocated in heap: total amount of memory allocated
* If max residency is high: usually a space leak
* If allocations are high, usually GC pressure from lots of temporary
  objects

## Profiling

```
$ stack exec -- ghc Main.hs -rtsopts -fforce-recomp -prof
$ ./Main +RTS -p -hc
$ stack exec -- hp2ps -e8in -c Main.hp
$ open Main.ps
```

![](http://i.imgur.com/G8SsJxv.png)

* Linear increase in memory used by lists and `Int`s
* Then it starts going down
* Any guesses now?

## Looking at core

Let's see what GHC compiles this into:

```
$ stack exec -- ghc Main.hs -rtsopts -fforce-recomp -ddump-simpl -ddump-to-file
```

Excerpt from `Main.dump-simpl`:

```haskell
-- RHS size: {terms: 6, types: 1, coercions: 0}
nums_r4a2 :: [Int]
[GblId, Str=DmdType]
nums_r4a2 =
  enumFromTo
    @ Int GHC.Enum.$fEnumInt (GHC.Types.I# 1#) (GHC.Types.I# 10000000#)

-- RHS size: {terms: 17, types: 10, coercions: 0}
main :: IO ()
[GblId, Str=DmdType]
main =
  print
    @ Double
    GHC.Float.$fShowDouble
    (/ @ Double
       GHC.Float.$fFractionalDouble
       (fromIntegral
          @ Int
          @ Double
          GHC.Real.$fIntegralInt
          GHC.Float.$fNumDouble
          (sum
             @ [] Data.Foldable.$fFoldable[] @ Int GHC.Num.$fNumInt nums_r4a2))
       (fromIntegral
          @ Int
          @ Double
          GHC.Real.$fIntegralInt
          GHC.Float.$fNumDouble
          (length @ [] Data.Foldable.$fFoldable[] @ Int nums_r4a2)))
```

OK, now everyone can guess.

## Solution 1: two lists

Let's trick GHC into not keeping the list in memory.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

main :: IO ()
main =
  let nums1 = [1..10000000 :: Int]
      nums2 = [1..10000000 :: Int]
   in print $ fromIntegral (sum nums1) / fromIntegral (length nums2)
```

Some stats:

```
$ stack exec -- ghc -O2 Main.hs -rtsopts -fforce-recomp -ddump-simpl -ddump-to-file && ./Main +RTS -s
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
5000000.5
         102,944 bytes allocated in the heap
           3,480 bytes copied during GC
          44,384 bytes maximum residency (1 sample(s))
          17,056 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0         0 colls,     0 par    0.000s   0.000s     0.0000s    0.0000s
  Gen  1         1 colls,     0 par    0.000s   0.003s     0.0027s    0.0027s

  INIT    time    0.000s  (  0.003s elapsed)
  MUT     time    0.014s  (  0.019s elapsed)
  GC      time    0.000s  (  0.003s elapsed)
  EXIT    time    0.000s  (  0.003s elapsed)
  Total   time    0.015s  (  0.027s elapsed)

  %GC     time       1.0%  (10.0% elapsed)

  Alloc rate    7,549,981 bytes per MUT second

  Productivity  98.5% of total user, 54.3% of total elapsed
```

The core for this
is... hairy. <https://gist.github.com/snoyberg/a26b4086834f7e5c39d75c55729f6c46>

However: Running without `-O2` didn't turn out so well. Also, the code
is ugly/repetitive. Let's try some additional approaches.

## vector

NOTE (from 2017): While preparing this bit, I discovered a bug in vector that's
been fixed upstream but not yet released
<https://github.com/haskell/vector/issues/111>.

2018: Still seems to be a problem

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.Vector.Unboxed as V

main :: IO ()
main =
  let nums1 = V.enumFromTo (1 :: Int) 10000000
      nums2 = V.enumFromTo (1 :: Int) 10000000
   in print $ fromIntegral (V.sum nums1) / fromIntegral (V.length nums2)
```

## List foldl'

```
import Data.List (foldl')

data State = State
    { stateSum :: Int
    , stateLen :: Int
    }

incrState :: State -> Int -> State
incrState (State s l) x = State (s + x) (l + 1)

avgState :: State -> Double
avgState (State s l) = fromIntegral s / fromIntegral l

main :: IO ()
main = print $ avgState $ foldl' incrState (State 0 0) [1..10000000]
```

__Question__ What's wrong with this code?

Let's analyze core for this:

<https://gist.github.com/snoyberg/0042a6653367366e5b789e70abf7d673>

And the important bits only:

```haskell
Main.main2 =
  case Main.$wgo 1# 0# 0#
  of _ [Occ=Dead] { (# ww1_s6SB, ww2_s6SC #) ->
  case GHC.Prim./##
         (GHC.Prim.int2Double# ww1_s6SB) (GHC.Prim.int2Double# ww2_s6SC)
  of wild2_a4qy { __DEFAULT ->
  GHC.Float.$w$sshowSignedFloat
    GHC.Float.$fShowDouble_$sshowFloat
    GHC.Show.shows22
    wild2_a4qy
    (GHC.Types.[] @ Char)
  }
  }

Main.$wgo =
  \ (w_s6Sp :: GHC.Prim.Int#)
    (ww_s6St [OS=OneShot] :: GHC.Prim.Int#)
    (ww1_s6Su [OS=OneShot] :: GHC.Prim.Int#) ->
    case w_s6Sp of wild_XC {
      __DEFAULT ->
        Main.$wgo
          (GHC.Prim.+# wild_XC 1#)
          (GHC.Prim.+# ww_s6St wild_XC)
          (GHC.Prim.+# ww1_s6Su 1#);
      10000000# ->
        (# GHC.Prim.+# ww_s6St 10000000#, GHC.Prim.+# ww1_s6Su 1# #)
    }
end Rec }
```

## foldl

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

import qualified Control.Foldl as L

average :: L.Fold Int Double
average = (/) <$> (fromIntegral <$> L.sum) <*> L.genericLength

main :: IO ()
main = print $ L.fold average [1..10000000 :: Int]
```

* Correct-by-construction
* Can't accidentally keep the list in memory

## conduit

* Same basic idea
* foldl will probably be faster

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

import Conduit

main :: IO ()
main = print
     $ runConduitPure
     $ enumFromToC 1 (10000000 :: Int)
    .| getZipSink ((/)
         <$> ZipSink (fromIntegral <$> sumC)
         <*> ZipSink lengthC)
```

```
$ stack exec -- ghc -O2 foo.hs && ./foo +RTS -s
[1 of 1] Compiling Main             ( foo.hs, foo.o )
Linking foo ...
5000000.5
   5,520,104,112 bytes allocated in the heap
       2,551,640 bytes copied during GC
          44,384 bytes maximum residency (2 sample(s))
          34,088 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)
```

* Constant memory
* Doesn't fuse away intermediate structures
* Lots of garbage created

## Unneeded here: straight loop

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE BangPatterns #-}

main :: IO ()
main =
    let loop idx total len =
          let !total' = total + idx
              !len' = len + 1
           in if idx == 10000000
                 then fromIntegral total' / fromIntegral len' :: Double
                 else loop (idx + 1) total' len'
     in print $ loop 1 0 0
```

Result:

```
$ stack exec -- ghc -O2 foo.hs && ./foo +RTS -s
[1 of 1] Compiling Main             ( foo.hs, foo.o )
Linking foo ...
5000000.5
     480,102,896 bytes allocated in the heap
          57,696 bytes copied during GC
          44,384 bytes maximum residency (2 sample(s))
          21,152 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)
```

Why so much allocation? (Real question, I didn't expect this.)

```haskell
Main.$wloop =
  \ (w_s5bV :: Integer) (w1_s5bW :: Integer) (w2_s5bX :: Integer) ->
    case integer-gmp-1.0.0.1:GHC.Integer.Type.plusInteger
           w2_s5bX Main.main4
    of len'_a1PN { __DEFAULT ->
    case integer-gmp-1.0.0.1:GHC.Integer.Type.plusInteger
           w1_s5bW w_s5bV
    of total'_a1ZJ { __DEFAULT ->
    case integer-gmp-1.0.0.1:GHC.Integer.Type.eqInteger#
           w_s5bV lvl_r5cV
    of wild_a2In { __DEFAULT ->
    case GHC.Prim.tagToEnum# @ Bool wild_a2In of _ [Occ=Dead] {
      False ->
        Main.$wloop
          (integer-gmp-1.0.0.1:GHC.Integer.Type.plusInteger
             w_s5bV Main.main4)
          total'_a1ZJ
          len'_a1PN;
      True ->
        case integer-gmp-1.0.0.1:GHC.Integer.Type.doubleFromInteger
               total'_a1ZJ
        of wild2_a2Jl { __DEFAULT ->
        case integer-gmp-1.0.0.1:GHC.Integer.Type.doubleFromInteger
               len'_a1PN
        of wild3_X2JV { __DEFAULT ->
        GHC.Prim./## wild2_a2Jl wild3_X2JV
        }
        }
    }
    }
    }
    }
end Rec }
```

Doh, it's using `Integer` instead of `Int`:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE BangPatterns #-}

main :: IO ()
main =
    let loop idx total len =
          let !total' = total + idx :: Int
              !len' = len + 1 :: Int
           in if idx == 10000000
                 then fromIntegral total' / fromIntegral len' :: Double
                 else loop (idx + 1) total' len'
     in print $ loop 1 0 0
```

```
         102,944 bytes allocated in the heap
           3,480 bytes copied during GC
          44,384 bytes maximum residency (1 sample(s))
          17,056 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)
```

Notice how close our code here looks to what GHC optimized out for us?

## Primitive types (really not necessary!)

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE MagicHash #-}
import GHC.Prim
import GHC.Magic
import GHC.Types

main :: IO ()
main =
    let loop idx total len =
          case total +# idx of
            total' ->
              case len +# 1# of
                len' ->
                  case idx of
                    10000000# -> D# (int2Double# total' /## int2Double# len')
                    _ -> loop (idx +# 1#) total' len'
     in print $ loop 1# 0# 0#
```

```
         102,944 bytes allocated in the heap
           3,480 bytes copied during GC
          44,384 bytes maximum residency (1 sample(s))
          17,056 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)
```

_Exactly_ the same. Yay!

__Exercise__ Compare the core for the previous two examples.

## Core for vector

Let's pull up an old example:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE FlexibleContexts #-}
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as V

myFunc :: V.Vector v Int => v Int -> IO ()
myFunc = V.mapM_ print . V.map (* 2) . V.filter odd

main :: IO ()
main = do
  myFunc $ VB.enumFromTo 1 10
```

Does this allocate a vector? Let's look at the core:

```
$ stack exec -- ghc -O2 -fforce-recomp foo.hs -ddump-to-file -ddump-simpl && ./foo +RTS -s
```

Whole thing:
<https://gist.github.com/snoyberg/2210c4f67c5d91186501d8a8d3c570e3>. Relevant
part:

```
Main.main_$s$wconsume_loop =
  \ (sc_savi [OS=OneShot] :: GHC.Prim.State# GHC.Prim.RealWorld)
    (sc1_savh :: GHC.Prim.Int#) ->
    case GHC.Prim.tagToEnum# @ Bool (GHC.Prim.<=# sc1_savh 10#)
    of _ [Occ=Dead] {
      False -> (# sc_savi, GHC.Tuple.() #);
      True ->
        case GHC.Prim.remInt# sc1_savh 2# of _ [Occ=Dead] {
          __DEFAULT ->
            case GHC.IO.Handle.Text.hPutStr2
                   GHC.IO.Handle.FD.stdout
                   (case GHC.Show.$wshowSignedInt
                           0# (GHC.Prim.*# sc1_savh 2#) (GHC.Types.[] @ Char)
                    of _ [Occ=Dead] { (# ww5_a9Pv, ww6_a9Pw #) ->
                    GHC.Types.: @ Char ww5_a9Pv ww6_a9Pw
                    })
                   GHC.Types.True
                   sc_savi
            of _ [Occ=Dead] { (# ipv_X77v, ipv1_X77x #) ->
            Main.main_$s$wconsume_loop ipv_X77v (GHC.Prim.+# sc1_savh 1#)
            };
          0# -> Main.main_$s$wconsume_loop sc_savi (GHC.Prim.+# sc1_savh 1#)
        }
    }
end Rec }
```

QED, no arrays allocated. But let's try another trick:

```
$ stack exec -- ghc -O2 -fforce-recomp foo.hs -ddump-to-file -ddump-rule-firings
```

Lots of output... if you know the internal GHC rewrite rules, it can
be informative. In particular, this is encouraging:

```
Rule fired: stream/unstream [Vector]
Rule fired: stream/unstream [Vector]
Rule fired: inplace/inplace [Vector]
Rule fired: stream/unstream [Vector]
```

The full output: <https://gist.github.com/snoyberg/53b1257507d0b2d5ffc6124ba5ac97de>

## Exercise

Section exercise: write a Gauge benchmark suite to test various
implementations of `average` against each other.

__Extra credit__ Also use weigh to measure allocations:
<https://www.stackage.org/package/weigh>
