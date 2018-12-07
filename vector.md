# vector

Section exercise: use mutable vectors to write a program that will
deal you a random hand of poker. Bonus: use an unboxed vector. Double
bonus: minimize the memory representation.

* Just like lists, but not
* Packed representation
* Spine strict (sometimes value strict)
* Plus a mutable interface

## The Three Types

* __Boxed__ Array of pointers
    * `Data.Vector`
* __Storable__ Binary rep, *pinned*
    * `Data.Vector.Storable`
* __Unboxed__ Binary rep, *unpinned*
    * `Data.Vector.Unboxed`
* Also has a generic interface unifying the others (more on that later)
* Ignore `Primitive`, use `Unboxed`

## Pinned vs unpinned

* Pinned
    * `malloc`/`free`
    * GC cannot move it around
    * Safe to pass to FFI
    * Can lead to memory fragmentation
* Unpinned
    * Fully managed by GC/RTS
    * Can be compacted by GC
    * Cannot be passed directly to FFI

## Compared to lists

* Less pointer indirection
* Unboxed and storable: no pointer indirection
* Less memory used for holding pointers
* Consing is more expensive
* No laziness/infinite data structures

```
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.Vector as V
import UnliftIO.Exception (tryAny)

main :: IO ()
main = do
    let list = [1..10] :: [Int]
        vector = V.fromList list :: V.Vector Int
        vector2 = V.enumFromTo 1 10 :: V.Vector Int
    print $ vector == vector2 -- True
    print $ list == V.toList vector -- also True
    print $ V.filter odd vector -- 1,3,5,7,9
    print $ V.map (* 2) vector -- 2,4,6,...,20
    print $ V.zip vector vector -- (1,1),(2,2),...(10,10)
    print $ V.zipWith (*) vector vector -- (1,4,9,16,...,100)
    print $ V.reverse vector -- 10,9,...,1
    print $ V.takeWhile (< 6) vector -- 1,2,3,4,5
    print $ V.takeWhile odd vector -- 1
    print $ V.takeWhile even vector -- []
    print $ V.dropWhile (< 6) vector -- 6,7,8,9,10
    print $ V.head vector -- 1
    print $ V.tail vector -- 2,3,4,...,10
    tryAny (return $! V.head $ V.takeWhile even vector) >>= print
```

* Advice: most cases, vectors are what you want
* Why are lists so ubiquitous?
    * Prelude encourages them
    * In base
    * Built-in syntax
    * Legitimately useful as a control structure (infinite lists)

## Mutable vs immutable

* Both mutable and immutable interfaces
* Mutable actions from inside `IO` or `ST`
* Freeze a mutable to an immutable
* Thaw an immutable to a mutable
* Immutable API: similar to lists
* Mutable API: similar to C arrays

## What do I use?

* Unless you know otherwise: immutable
* If unboxed is possible, use it
* Otherwise, if storable is possible, use it
* Otherwise, use boxed
* Generic algorithm? Use `Generic`
* Polymorphic container? Stick with boxed

## Stream fusion

* Avoid intermediate data structures
* Leverages stream representation and rewrite rules
* Only applies to immutable API
* When it works, gives great speedups

```haskell
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = print $ V.sum $ V.enumFromTo 1 (10^9 :: Int)
```

## Slicing

* `take`, `break`, etc, all take slices of existing memory buffers
* Does not require any new allocations or copying
* `append`, `cons`, etc, do create new allocations

## Vector basics

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.Vector as V

main :: IO ()
main = V.mapM_ print
     $ V.map (* 2)
     $ V.filter odd
     $ V.enumFromTo 1 (10 :: Int)
```

### Unboxed

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = V.mapM_ print
     $ V.map (* 2)
     $ V.filter odd
     $ V.enumFromTo 1 (10 :: Int)
```

* Exericse: what's the difference?
* Rewrite to use storable vectors

## Generic API

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
  myFunc $ VS.enumFromTo 1 10
  myFunc $ VU.enumFromTo 1 10
```

Trick question: which is the most efficient? (We'll look at core code
to prove this later.)

## Strictness

Guess the output:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE FlexibleContexts #-}
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import UnliftIO.Exception (pureTry)

main :: IO ()
main = do
  print $ pureTry $ VB.head $ VB.fromList (():undefined)
  print $ pureTry $ VS.head $ VS.fromList (():undefined)
  print $ pureTry $ VU.head $ VU.fromList (():undefined)

  print $ pureTry $ VB.head $ VB.fromList [(), undefined]
  print $ pureTry $ VS.head $ VS.fromList [(), undefined]
  print $ pureTry $ VU.head $ VU.fromList [(), undefined]
```

* Boxed: spine strict
* Storable and unboxed: value strict

__Question__ Why does this difference exist?

## Mutable vectors

* Imperative API
* Generalized to `PrimMonad` (`IO`, `ST`, and transformers)
* Construct
    * `new :: PrimMonad m => Int -> m (MVector (PrimState m) a)`
    * `replicate :: PrimMonad m => Int -> a -> m (MVector (PrimState m) a)`
    * `replicateM :: replicateM :: PrimMonad m => Int -> m a -> m (MVector (PrimState m) a)`
* Individual elements
    * `read :: PrimMonad m => MVector (PrimState m) a -> Int -> m a`
    * `write :: :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()`
    * `modify :: PrimMonad m => MVector (PrimState m) a -> (a -> a) -> Int -> m ()`
    * `swap :: PrimMonad m => MVector (PrimState m) a -> Int -> Int -> m ()`
* Converting to/from immutable
    * `freeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)`
    * `thaw :: PrimMonad m => Vector a -> m (MVector (PrimState m) a)`
    * `unsafeFreeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)`
    * `unsafeThaw :: PrimMonad m => Vector a -> m (MVector (PrimState m) a)`
    * __Question__ What's the runtime of the safe vs unsafe versions?
* Creation helper
    * `create :: (forall s. ST s (MVector s a)) -> Vector a`
    * `modify :: (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a`

__Exercise__ Implement `create` and `modify`

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE FlexibleContexts #-}
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import UnliftIO.Exception
import GHC.Prim (RealWorld)

-- type constrained to avoid ambiguities
printVector :: VM.MVector RealWorld Int -> IO ()
printVector mv = do
  putStrLn "***Begin printVector***"
  v <- V.freeze mv
  print v `catchAny` print
  putStrLn "***End printVector***\n"

main :: IO ()
main = do
  mv <- VM.new 10
  printVector mv

  VM.write mv 0 100
  printVector mv

  VM.read mv 0 >>= print

  VM.modify mv (+ 1) 0
  printVector mv

  print $ V.create $ do
    mv <- VM.replicate 10 (10 :: Int)
    VM.modify mv (* 2) 0
    VM.swap mv 0 1
    return mv

  let v = V.enumFromTo 11 (20 :: Int)
  print $ V.modify (\mv -> VM.swap mv 0 1) v
```

__Question__ What would happen if we switched to unboxed or storable
vectors?

## vector-algorithms

__Exercise__ Fill a vector with 100 random integers between 1 and
10000 and sort it. Use vector-algorithms.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.Vector.Unboxed as V
import Data.Vector.Algorithms.Insertion (sort)
import System.Random (randomRIO)

main :: IO ()
main = do
  v <- V.replicateM 100 $ randomRIO (1, 10000 :: Int)
  print $ V.modify sort v
```

Or

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Vector.Algorithms.Insertion (sort)
import System.Random

main :: IO ()
main = do
  gen0 <- getStdGen
  print $ V.create $ do
    mv <- VM.new 100
    let loop gen idx
          | idx >= 100 = return ()
          | otherwise = do
              let (x, gen') = randomR (1, 10000) gen
              VM.write mv idx (x :: Int)
              loop gen' (idx + 1)
    loop gen0 0
    sort mv
    return mv
```

Or

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Vector.Algorithms.Insertion (sort)
import System.Random
import Data.Foldable (forM_)

main :: IO ()
main = do
  mv <- VM.new 100
  forM_ [0..99] $ \idx -> do
    x <- randomRIO (1, 10000)
    VM.write mv idx (x :: Int)
  sort mv
  v <- V.unsafeFreeze mv
  print v
```

## Exercises

Test the randomness of `System.Random`: use `randomRIO (0, 9)`
repeatedly to generate a random values and see if the distribution is
close to uniform. First use immutable vectors:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import           Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as V
import           System.Random       (randomRIO)

main :: IO ()
main = do
    let v0 = V.replicate 10 (0 :: Int)

        loop v 0 = return v
        loop v rest = do
            i <- randomRIO (0, 9)
            let oldCount = v ! i
                v' = v // [(i, oldCount + 1)]
            loop v' (rest - 1)

    vector <- loop v0 (10^6)
    print vector
```

__Question__ Is this efficient?

Now use mutable vectors:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import           Control.Monad               (replicateM_)
import           Data.Vector.Unboxed         (freeze)
import qualified Data.Vector.Unboxed.Mutable as V
import           System.Random               (randomRIO)

main :: IO ()
main = do
    vector <- V.replicate 10 (0 :: Int)

    replicateM_ (10^6) $ do
        i <- randomRIO (0, 9)
        oldCount <- V.read vector i
        V.write vector i (oldCount + 1)

    ivector <- freeze vector
    print ivector
```

Calculate the frequency of each byte (0-255) for the content coming
from standard input.

```haskell
{-# LANGUAGE FlexibleContexts #-}
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Data.ByteString.Lazy        as L
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
import           Data.Word                   (Word8)

main :: IO ()
main = do
    -- Get all of the contents from stdin
    lbs <- L.getContents

    -- Create a new 256-size mutable vector
    -- Fill the vector with zeros
    mutable <- M.replicate 256 0

    -- Add all of the bytes from stdin
    addBytes mutable lbs

    -- Freeze to get an immutable version
    vector <- U.unsafeFreeze mutable

    -- Print the frequency of each byte
    -- In newer vectors: we can use imapM_
    U.zipWithM_ printFreq (U.enumFromTo 0 255) vector

addBytes :: (PrimMonad m, M.MVector v Int)
         => v (PrimState m) Int
         -> L.ByteString
         -> m ()
addBytes v lbs = mapM_ (addByte v) (L.unpack lbs)

addByte :: (PrimMonad m, M.MVector v Int)
        => v (PrimState m) Int
        -> Word8
        -> m ()
addByte v w = do
    -- Read out the old count value
    oldCount <- M.read v index
    -- Write back the updated count value
    M.write v index (oldCount + 1)
  where
    -- Indices in vectors are always Ints. Our bytes come in as Word8, so we
    -- need to convert them.
    index :: Int
    index = fromIntegral w

printFreq :: Int -> Int -> IO ()
printFreq index count = putStrLn $ concat
    [ "Frequency of byte "
    , show index
    , ": "
    , show count
    ]
```

## Section exercise

* Use `mwc-random` package
    * Not a recommendation for random packages, just a good way to
      practice vectors
* May want to consider:
  [vector-th-unbox](https://www.stackage.org/package/vector-th-unbox)
* Note: that won't provide the tightest representation!
* Hard core: write an `Unbox` instance by hand
* Less hard core (what I'd probably do): can you use
  `GeneralizedNewtypeDeriving`?
