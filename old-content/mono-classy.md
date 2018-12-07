# mono-traversable and classy-prelude

## Where Foldable fails

Does this work?

```haskell
-- expected to fail #!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Text as T

main :: IO ()
main = do
  let string = "hello"
      vector = V.fromList string
      text   = T.pack string
  F.mapM_ print string
  F.mapM_ print vector
  F.mapM_ print text
```

Nope

```
14:17: error:
    • Couldn't match expected type ‘t0 a0’ with actual type ‘T.Text’
    • In the second argument of ‘mapM_’, namely ‘text’
      In a stmt of a 'do' block: mapM_ print text
      In the expression:
        do { let string = "hello"
                 vector = V.fromList string
                 ....;
             mapM_ print string;
             mapM_ print vector;
             mapM_ print text }
```

Why? `Text` is monomorphic, and therefore has kind `*`. `Foldable`
needs something with kind `* -> *` (like a list or `Vector`).

Let's try an unboxed vector:

```haskell
import qualified Data.Vector.Unboxed as V
```

This also fails.

```
13:3: error:
    • No instance for (Foldable V.Vector) arising from a use of ‘mapM_’
    • In a stmt of a 'do' block: mapM_ print vector
      In the expression:
        do { let string = "hello"
                 vector = V.fromList string
                 ....;
             mapM_ print string;
             mapM_ print vector }
      In an equation for ‘main’:
          main
            = do { let string = ...
                       ....;
                   mapM_ print string;
                   mapM_ print vector }
```

Why? We need an `Unbox` constraint on the values, and can't guarantee
that in the instance declaration:

```haskell
instance Unbox ? => Foldable V.Vector where
```

## Generalizing Foldable

* Make `MonoFoldable` as a monomorphic class
* Requires types of kind `*`
* We can add constraints to values
* We can use monomorphic types
* Use TypeFamilies to make it happen

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE TypeFamilies #-}
import qualified Data.Foldable as F
import qualified Data.Vector.Unboxed as V
import qualified Data.Text as T
import Data.Monoid (Endo (..))

type family Element mono

class MonoFoldable mono where
  ofoldMap :: Monoid m => (Element mono -> m) -> mono -> m

type instance Element [a] = a
instance MonoFoldable [a] where
  ofoldMap = F.foldMap

type instance Element (V.Vector a) = a
instance V.Unbox a => MonoFoldable (V.Vector a) where
  ofoldMap f = V.foldr (mappend . f) mempty

type instance Element T.Text = Char
instance MonoFoldable T.Text where
  ofoldMap f = T.foldr (mappend . f) mempty

omapM_ :: (Applicative m, MonoFoldable mono)
       => (Element mono -> m ())
       -> mono
       -> m ()
omapM_ f mono = appEndo
  (ofoldMap (\x -> (Endo (f x *>))) mono)
  (pure ())

main :: IO ()
main = do
  let string = "hello"
      vector = V.fromList string
      text   = T.pack string
  omapM_ print string
  omapM_ print vector
  omapM_ print text
```

Advantages:

* Strictly more general than `Foldable`
* Get a unified interface for many different types
* Avoid a bunch of qualified imports (if you're allergic to them)
* Suddenly have functions we never had before (e.g., `Data.Text.mapM_`
  does not exist)

Disadvantages:

* More complicated than straight `Foldable`
* Not exported by `Prelude` (we'll get to that)

## Why no MonoFunctor?

Sure, let's do it!

```haskell
-- Does not compile
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE TypeFamilies #-}

type family Element mono

class MonoFunctor mono where
  omap :: (Element mono -> Element mono) -> mono -> mono

type instance Element [a] = a
instance MonoFunctor [a] where
  omap = map

main :: IO ()
main = do
  let string = "hello"
  print $ omap succ string
  print $ omap show [1..5 :: Int]
```

Oops:

```
18:16: error:
    • Couldn't match type ‘[Char]’ with ‘Int’
      Expected type: Element [Int] -> Element [Int]
        Actual type: Int -> String
    • In the first argument of ‘omap’, namely ‘show’
      In the second argument of ‘($)’, namely ‘omap show [1 .. 5 :: Int]’
      In a stmt of a 'do' block: print $ omap show [1 .. 5 :: Int]
```

`MonoFunctor` and `MonoTraversable` are _not_ as general as `Functor`
and `Traversable`: they cannot change the contained type. Takeaway:
`MonoFoldable` is really cool, others are less cool.

## mono-traversable package

* Built around monomorphising `Traversable` hierarchy
* Namespaces by adding `o` to the beginning
* Branched out to other monomorphic interfaces

## Data.NonNull

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.NonNull as N
import Data.MonoTraversable (opoint)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Monoid ((<>))

main :: IO ()
main = do
  TIO.putStrLn "Enter your name"
  name <- TIO.getLine
  case N.fromNullable name of
    Nothing -> TIO.putStrLn "You didn't provide a name!"
    Just nonNull ->
      TIO.putStrLn $ "Last letter: " <> opoint (N.last nonNull)
```

* More general than `NonEmpty` (works on things besides lists)
* More efficient than `(a, Vector a)` or similar
* Avoids partial functions!

## Data.Sequences

* General interface for list-like things
* Lists, vectors, `ByteString`, and `Text`
* `SemiSequence`: functions that work with `NonNull`
* `Sequence`: functions that may generate null values (like `filter`)

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import           UnliftIO.Exception (tryAny)
import           Data.MonoTraversable
import           Data.NonNull
import           Data.Sequences
import           Data.Vector.Storable   (enumFromTo)
import           Prelude                (Eq (..), IO, Int, Monad (..), Num (..),
                                         Ord (..), even, odd, print, ($), ($!))

main :: IO ()
main = do
    let vector = enumFromTo 1 (10 :: Int)
    print $ filter odd vector -- 1,3,5,7,9
    print $ omap (* 2) vector -- 2,4,6,...,20
    print $ reverse vector -- 10,9,...,1
    print $ takeWhile (< 6) vector -- 1,2,3,4,5
    print $ takeWhile odd vector -- 1
    print $ takeWhile even vector -- []
    print $ dropWhile (< 6) vector -- 6,7,8,9,10

    let nvector = impureNonNull vector
    print $ head nvector -- 1
    print $ tail nvector -- 2,3,4,...,10

    let nvector2 = impureNonNull $ takeWhile even vector
    tryAny (return $! head nvector2) >>= print
```

## Data.Containers

* Unifies `Map` and `Set` APIs
* Not quite as general as `Data.Map` (e.g., `difference`)
* Makes it much easier to switch between `Map` and `HashMap`
* Uses value-strict versions

## classy-prelude

Annoyed by all of those imports? Say no more!

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE NoImplicitPrelude #-}
import           ClassyPrelude

main :: IO ()
main = do
    let vector = pack [1..10] :: SVector Int
    print $ filter odd vector -- 1,3,5,7,9
    print $ omap (* 2) vector -- 2,4,6,...,20
    print $ reverse vector -- 10,9,...,1
    print $ takeWhile (< 6) vector -- 1,2,3,4,5
    print $ takeWhile odd vector -- 1
    print $ takeWhile even vector -- []
    print $ dropWhile (< 6) vector -- 6,7,8,9,10

    let nvector = impureNonNull vector
    print $ head nvector -- 1
    print $ tail nvector -- 2,3,4,...,10

    let nvector2 = impureNonNull $ takeWhile even vector
    tryAny (return $! head nvector2) >>= print
```

* Generalizes functions to classes where possible
* Prefers `MonoFoldable`, `Data.Sequences`, and `Data.Containers`
* Avoids partial functions like the plague
* No lazy I/O either (I hope...)
* Very opinionated, mostly my opinions :)
