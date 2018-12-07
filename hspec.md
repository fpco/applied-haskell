# hspec

* Behavior testing
* IMO, just a nice test framework
* tasty is also popular

## Get started

```
$ stack new mylib rio --resolver lts-12.21
$ cd mylib
$ stack test
...
mylib-0.1.0.0: Test suite mylib-test passed
```

Woohoo, let's kick this off

## Test driven development

Create an `src/Reverse.hs`:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
module Reverse
  ( myReverse
  ) where
import RIO

myReverse :: [a] -> [a]
myReverse = undefined
```

Create `test/ReverseSpec.hs`

```haskell
module ReverseSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Reverse

spec :: Spec
spec = do
  describe "myReverse" $ do
    it "handles empty lists" $ myReverse [] `shouldBe` ([] :: [Int])
```

And now run in a terminal:

```
$ stack test --file-watch --fast
```

It fails, of course:

```
Failures:
  test/ReverseSpec.hs:10:
  1) Reverse.myReverse handles empty lists
       uncaught exception: ErrorCall (Prelude.undefined
       CallStack (from HasCallStack):
         error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
         undefined, called at src/Reverse.hs:9:13 in mylib-0.1.0.0-JvPVOgHHcOeAAaDSh8A4PO:Reverse)
```

Fix it...

```haskell
myReverse :: [a] -> [a]
myReverse [] = []
```

Tests pass. Add another test...

```haskell
    it "reverses hello" $ myReverse "hello" `shouldBe` "olleh"
```

It breaks, add more code:

```haskell
myReverse (x:xs) = myReverse xs ++ [x]
```

Yay! Let's add a property:

```haskell
prop "double reverse is id" $ \list ->
    myReverse (myReverse list) `shouldBe` (list :: [Int])
```

That's inefficient, let's create a better reverse function. Again,
TDD:

```haskell
-- code
betterReverse :: [a] -> [a]
betterReverse = undefined

-- test
describe "betterReverse" $ do
  prop "behaves the same as myReverse" $ \list ->
    betterReverse list `shouldBe` myReverse (list :: [Int])
```

__Exercise__ Why is `myReverse` slow, and how can you make it faster?

Real buggy code I wrote by mistake...

```haskell
betterReverse :: [a] -> [a]
betterReverse =
    loop []
  where
    loop res [] = []
    loop res (x:xs) = loop (x:res) xs
```

Test suite catches me!

```
  test/ReverseSpec.hs:16:
  1) Reverse.betterReverse behaves the same as myReverse
       Falsifiable (after 2 tests and 2 shrinks):
       expected: [0]
        but got: []
       [0]
```

OK, let's fix the code:

```haskell
betterReverse :: [a] -> [a]
betterReverse =
    loop []
  where
    loop res [] = res
    loop res (x:xs) = loop (x:res) xs
```

Hurrah!

## Let's play with vector

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
module Reverse
  ( myReverse
  , betterReverse
  , vectorReverse
  , uvectorReverse
  , svectorReverse
  ) where

import RIO
import qualified RIO.Vector as V
import qualified RIO.Vector.Boxed as VB
import qualified RIO.Vector.Storable as VS
import qualified RIO.Vector.Unboxed as VU
import qualified Data.Vector.Generic.Mutable as VM

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

betterReverse :: [a] -> [a]
betterReverse =
    loop []
  where
    loop res [] = res
    loop res (x:xs) = loop (x:res) xs

vectorReverseGeneric
  :: V.Vector v a
  => [a]
  -> v a
vectorReverseGeneric input = V.create $ do
  let len = length input
  v <- VM.new len
  let loop [] idx = assert (idx == -1) (return v)
      loop (x:xs) idx = do
        VM.unsafeWrite v idx x
        loop xs (idx - 1)
  loop input (len - 1)
{-# INLINEABLE vectorReverseGeneric #-}

vectorReverse :: [a] -> [a]
vectorReverse = VB.toList . vectorReverseGeneric
{-# INLINE vectorReverse #-}

svectorReverse :: VS.Storable a => [a] -> [a]
svectorReverse = VS.toList . vectorReverseGeneric
{-# INLINE svectorReverse #-}

uvectorReverse :: VU.Unbox a => [a] -> [a]
uvectorReverse = VU.toList . vectorReverseGeneric
{-# INLINE uvectorReverse #-}
```

Awesome, but is it faster? Let's add a benchmark!

```
benchmarks:
  reverse-bench:
    main: reverse-bench.hs
    source-dirs: bench
    dependencies:
    - mylib
    - gauge

    ghc-options:
    - -O2
```

And `bench/reverse.hs`

```haskell
import Lib
import Gauge

main :: IO ()
main =
    defaultMain $ map group [5, 100, 10000, 1000000]
  where
    group size =
        bgroup (show size)
          [ bench "reverse" $ whnf reverse list
          , bench "myReverse" $ whnf myReverse list
          , bench "betterReverse" $ whnf betterReverse list
          , bench "vectorReverse" $ whnf vectorReverse list
          , bench "svectorReverse" $ whnf svectorReverse list
          , bench "uvectorReverse" $ whnf uvectorReverse list
          ]
      where
        list = [1..size :: Int]
```

Results:

```
Benchmark reverse-bench: RUNNING...
benchmarking 5/reverse
time                 23.28 ns   (21.61 ns .. 25.46 ns)
                     0.967 R²   (0.939 R² .. 0.999 R²)
mean                 22.36 ns   (21.83 ns .. 23.78 ns)
std dev              2.680 ns   (1.041 ns .. 5.029 ns)
variance introduced by outliers: 94% (severely inflated)

benchmarking 5/myReverse
time                 44.75 ns   (44.26 ns .. 45.20 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 44.44 ns   (43.98 ns .. 44.86 ns)
std dev              1.413 ns   (1.185 ns .. 1.744 ns)
variance introduced by outliers: 50% (severely inflated)

benchmarking 5/betterReverse
time                 22.67 ns   (22.21 ns .. 23.28 ns)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 22.75 ns   (22.47 ns .. 23.08 ns)
std dev              1.055 ns   (836.9 ps .. 1.428 ns)
variance introduced by outliers: 70% (severely inflated)

benchmarking 5/vectorReverse
time                 424.6 ns   (413.9 ns .. 437.0 ns)
                     0.996 R²   (0.994 R² .. 0.999 R²)
mean                 416.7 ns   (411.3 ns .. 423.0 ns)
std dev              20.06 ns   (14.97 ns .. 27.91 ns)
variance introduced by outliers: 66% (severely inflated)

benchmarking 5/svectorReverse
time                 69.79 ns   (69.14 ns .. 70.36 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 69.76 ns   (69.20 ns .. 70.47 ns)
std dev              2.173 ns   (1.807 ns .. 2.647 ns)
variance introduced by outliers: 49% (moderately inflated)

benchmarking 5/uvectorReverse
time                 69.79 ns   (68.58 ns .. 71.32 ns)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 69.26 ns   (68.52 ns .. 70.17 ns)
std dev              2.928 ns   (2.210 ns .. 4.326 ns)
variance introduced by outliers: 64% (severely inflated)

benchmarking 100/reverse
time                 440.4 ns   (437.1 ns .. 443.8 ns)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 448.7 ns   (442.5 ns .. 471.6 ns)
std dev              34.27 ns   (13.84 ns .. 79.16 ns)
variance introduced by outliers: 83% (severely inflated)

benchmarking 100/myReverse
time                 1.020 μs   (997.1 ns .. 1.053 μs)
                     0.996 R²   (0.992 R² .. 0.999 R²)
mean                 1.020 μs   (1.008 μs .. 1.039 μs)
std dev              52.24 ns   (38.18 ns .. 74.27 ns)
variance introduced by outliers: 67% (severely inflated)

benchmarking 100/betterReverse
time                 432.5 ns   (429.5 ns .. 436.6 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 433.0 ns   (429.5 ns .. 438.0 ns)
std dev              13.61 ns   (11.12 ns .. 17.58 ns)
variance introduced by outliers: 45% (moderately inflated)

benchmarking 100/vectorReverse
time                 3.304 μs   (3.276 μs .. 3.344 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 3.383 μs   (3.306 μs .. 3.548 μs)
std dev              354.1 ns   (106.1 ns .. 609.7 ns)
variance introduced by outliers: 89% (severely inflated)

benchmarking 100/svectorReverse
time                 695.4 ns   (682.1 ns .. 708.0 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 693.5 ns   (686.7 ns .. 702.2 ns)
std dev              24.88 ns   (21.48 ns .. 30.24 ns)
variance introduced by outliers: 51% (severely inflated)

benchmarking 100/uvectorReverse
time                 677.0 ns   (669.2 ns .. 684.9 ns)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 676.3 ns   (669.5 ns .. 684.9 ns)
std dev              26.76 ns   (20.91 ns .. 39.11 ns)
variance introduced by outliers: 56% (severely inflated)

benchmarking 10000/reverse
time                 83.32 μs   (81.00 μs .. 86.26 μs)
                     0.992 R²   (0.985 R² .. 0.998 R²)
mean                 84.04 μs   (82.26 μs .. 87.09 μs)
std dev              7.867 μs   (4.523 μs .. 14.30 μs)
variance introduced by outliers: 80% (severely inflated)

benchmarking 10000/myReverse
time                 1.021 ms   (975.4 μs .. 1.085 ms)
                     0.968 R²   (0.939 R² .. 0.993 R²)
mean                 1.019 ms   (989.0 μs .. 1.067 ms)
std dev              125.8 μs   (81.76 μs .. 210.2 μs)
variance introduced by outliers: 80% (severely inflated)

benchmarking 10000/betterReverse
time                 80.50 μs   (79.97 μs .. 81.07 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 80.94 μs   (80.37 μs .. 81.84 μs)
std dev              2.240 μs   (1.670 μs .. 3.303 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking 10000/vectorReverse
time                 490.6 μs   (467.7 μs .. 516.9 μs)
                     0.974 R²   (0.953 R² .. 0.995 R²)
mean                 487.4 μs   (467.1 μs .. 526.0 μs)
std dev              88.08 μs   (53.06 μs .. 140.0 μs)
variance introduced by outliers: 91% (severely inflated)

benchmarking 10000/svectorReverse
time                 82.30 μs   (81.49 μs .. 83.28 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 84.65 μs   (83.64 μs .. 85.73 μs)
std dev              3.463 μs   (2.856 μs .. 4.741 μs)
variance introduced by outliers: 43% (moderately inflated)

benchmarking 10000/uvectorReverse
time                 82.09 μs   (80.97 μs .. 83.18 μs)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 83.02 μs   (81.81 μs .. 85.07 μs)
std dev              5.168 μs   (3.065 μs .. 8.483 μs)
variance introduced by outliers: 64% (severely inflated)

benchmarking 1000000/reverse
time                 60.50 ms   (56.80 ms .. 64.33 ms)
                     0.990 R²   (0.975 R² .. 0.997 R²)
mean                 60.70 ms   (58.23 ms .. 63.52 ms)
std dev              4.870 ms   (3.493 ms .. 6.662 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking 1000000/myReverse
time                 155.0 ms   (142.2 ms .. 166.9 ms)
                     0.991 R²   (0.958 R² .. 1.000 R²)
mean                 156.2 ms   (152.1 ms .. 165.9 ms)
std dev              8.219 ms   (1.470 ms .. 12.12 ms)
variance introduced by outliers: 13% (moderately inflated)

benchmarking 1000000/betterReverse
time                 61.28 ms   (56.93 ms .. 64.75 ms)
                     0.990 R²   (0.981 R² .. 0.997 R²)
mean                 60.47 ms   (58.42 ms .. 63.90 ms)
std dev              4.605 ms   (2.755 ms .. 6.999 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking 1000000/vectorReverse
time                 59.07 ms   (55.77 ms .. 62.18 ms)
                     0.994 R²   (0.989 R² .. 0.998 R²)
mean                 56.09 ms   (53.91 ms .. 58.04 ms)
std dev              3.582 ms   (2.581 ms .. 5.108 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking 1000000/svectorReverse
time                 12.78 ms   (12.16 ms .. 13.55 ms)
                     0.955 R²   (0.881 R² .. 0.996 R²)
mean                 13.49 ms   (13.09 ms .. 14.58 ms)
std dev              1.558 ms   (663.3 μs .. 2.871 ms)
variance introduced by outliers: 56% (severely inflated)

benchmarking 1000000/uvectorReverse
time                 12.04 ms   (11.72 ms .. 12.31 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 12.22 ms   (12.07 ms .. 12.37 ms)
std dev              388.4 μs   (306.0 μs .. 490.7 μs)
variance introduced by outliers: 10% (moderately inflated)
```

__Question__ What does this report tell you about the performance?
