# containers

See <string-types.md> for exercise for this section.

## Maps

* Dictionaries
* __Map__ping from keys to values
    * See what I did there?

## Sets

* `Map`s without values
* Think mathematical sets

## Flavors

* Ordered: binary tree, requires `Ord` on keys, logarithmic lookup
* Int: Int keys, slightly more efficient ordered
* Hashed: amortized O(1) operations, requires `Hashable` on keys
    * Usually considered to supercede `IntMap`/`IntSet` at this point
* Maps of strict and lazy APIs
    * Same data structures
    * Recommendation: use strict unless you know better

## The tutorial

<https://haskell-lang.org/library/containers>

# The rest of containers

* Not covering it here
* `Data.Sequence` (efficient append/prepend, very useful!)
* `Data.Graph`
* `Data.Tree`

## Exercises

Calculate the frequency of each byte available on standard
input. Example usage:

```
$ echo hello world | ./Main.hs
(10,1)
(32,1)
(100,1)
(101,1)
(104,1)
(108,3)
(111,2)
(114,1)
(119,1)
```

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  lbs <- BL.getContents
  let add m w = Map.insertWith (+) w 1 m
  mapM_ print $ Map.toList $ BL.foldl' add Map.empty lbs
```

Implement a `MultiMap` based on `Map` and `Set` with the following
signature and provides instances for `Show`, `Eq`, `Foldable`,
`Semigroup`, and `Monoid`.

```haskell
newtype MultiMap k v
insert :: (Ord k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
delete :: (Ord k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
deleteAll :: Ord k => k -> MultiMap k v -> MultiMap k v
lookup :: Ord k => k -> MultiMap k v -> Set v
member :: (Ord k, Ord v) => k -> v -> MultiMap k v -> Bool
```

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE DeriveFoldable #-}
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Semigroup
import Data.Maybe (fromMaybe)
import Test.Hspec
import Prelude hiding (lookup)

newtype MultiMap k v = MultiMap
  { toMap :: Map k (Set v)
  }
  deriving (Show, Eq, Foldable)

instance (Ord k, Ord v) => Semigroup (MultiMap k v) where
  MultiMap l <> MultiMap r = MultiMap $ Map.unionWith Set.union l r

instance (Ord k, Ord v) => Monoid (MultiMap k v) where
  mempty = MultiMap mempty
  mappend = (<>)

insert :: (Ord k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
insert k v (MultiMap m) =
  MultiMap $ Map.insertWith Set.union k (Set.singleton v) m

delete :: (Ord k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
delete k v (MultiMap m) =
  MultiMap $ Map.update fixSet k m
  where
    fixSet set0
      | Set.null set1 = Nothing
      | otherwise = Just set1
      where
        set1 = Set.delete v set0

deleteAll :: Ord k => k -> MultiMap k v -> MultiMap k v
deleteAll k (MultiMap m) = MultiMap $ Map.delete k m

lookup :: Ord k => k -> MultiMap k v -> Set v
lookup k (MultiMap m) = fromMaybe Set.empty $ Map.lookup k m

member :: (Ord k, Ord v) => k -> v -> MultiMap k v -> Bool
member k v (MultiMap m) = maybe False (v `Set.member`) $ Map.lookup k m

main :: IO ()
main = hspec $ do
  it "member on empty fails" $ member () () mempty `shouldBe` False
  it "insert/member" $ member () () (insert () () mempty) `shouldBe` True
  it "insert/lookup"
    $ lookup () (insert () () mempty) `shouldBe` Set.singleton ()
  it "deleteAll" $ deleteAll () (insert () () mempty) `shouldBe` mempty
  it "delete" $ delete () () (insert () () mempty) `shouldBe` mempty
  it "<>" $
    lookup () (insert () False mempty <> insert () True mempty) `shouldBe`
    Set.fromList [False, True]
```

* * *

*New content, merge with the old at some point*

## Maps

There are three core `Map`-like data types:

```haskell
data Map key value
data IntMap value -- Int is the key, always
data HashMap key value
```

`IntMap` is a specialized, optimized `Map Int`. In practice, it isn't
used very much (especially since `HashMap Int` tends to be
faster). `Map` is a binary tree, with O(log n) operations and an
ordered output. `HashMap` is (surprise) a hash map, with amortized
O(1) operations.

* `Map` requires `Ord` (and `Eq`) on keys
* `HashMap` requires Hashable and Eq

It's a lot easier to get an `Ord` instance:

```haskell
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Name = Name String
  deriving (Eq, Ord)

nameAgeMap :: Map Name Int
nameAgeMap = Map.fromList
  [ (Name "Alice", 25)
  , (Name "Bob", 30)
  , (Name "Charlie", 35)
  ]
```

By contrast, `HashMap` requires more work:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Name = Name String
  deriving (Eq, Generic)
instance Hashable Name -- uses Generic

nameAgeHashMap :: HashMap Name Int
nameAgeHashMap = HashMap.fromList
  [ (Name "Alice", 25)
  , (Name "Bob", 30)
  , (Name "Charlie", 35)
  ]
```

Generally: `HashMap`, so unless you have a reason to do otherwise, use
`HashMap`. Example reasons:

* For some reason you can't get a `Hashable` instance
* You want the results sorted in the end

Final note: if you defined `Name` above as `newtype Name = Name
String`, you _could_ do `deriving Hashable`, by turning on `{-#
LANGUAGE GeneralizedNewtypeDeriving #-}`.

## Strict vs lazy values

Summary: unless you really know what you're doing, always import the
`.Strict` modules.

Maps are always strict in their keys, meaning that forcing a map
always forcing all of the keys. That's because it's necessary to
analyze the keys themselves to put them into the hash table or binary
tree appropriately. However, it's not necessary to be strict in the
values. The default modules (`Data.Map` and `Data.IntMap`) are
_lazy_. Use the `.Strict` modules unless you have strong reason to do
otherwise.

## Mutability

Unlike other languages, Maps are immutable. This means you can safely
pass them to other threads and functions, you never worry about data
races, etc. The functions that "mutate" actually return brand new map
values. If you need mutation across threads, you need to use a mutable
variable like `IORef` or `TVar` (which we haven't covered yet).

Downside of immutability: HashMaps do have a performance
overhead. There's a mutable hashtables library, but it's not nearly as
well used as containers and unordered-containers.

## Map API overview

There are many other functions, but here's what I'd consider the core
API. Note that all three modules provide almost identical APIs, so
learning one (e.g. `HashMap`) helps you understand the others
(e.g. `Map`).

```haskell
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.HashMap.Strict as HashMap

singleton :: k -> v -> Map k v
fromList :: [(k, v)] -> Map k v
toList :: Map k v -> [(k, v)]
lookup :: k -> Map k v -> Maybe v
insert :: k -> v -> Map k v -> Map k v
insertWith :: (v -> v -> v) -> k -> v -> Map k v -> Map k v
union :: Map k v -> Map k v -> Map k v
unionWith :: (v -> v -> v) -> Map k v -> Map k v -> Map k v
```

## Exercises

You'll want to reference the APIs for `Map`s and `HashMap`s:

* https://www.stackage.org/haddock/lts-12.21/containers-0.5.7.1/Data-Map-Strict.html
* https://www.stackage.org/haddock/lts-12.21/unordered-containers-0.2.8.0/Data-HashMap-Strict.html

### Exercise 1

Implement the `printScore` function:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

printScore :: Map String Int -> String -> IO ()
printScore = _

main :: IO ()
main =
    mapM_ (printScore scores) ["Alice", "Bob", "David"]
  where
    scores :: Map String Int
    scores = Map.fromList
      [ ("Alice", 95)
      , ("Bob", 90)
      , ("Charlies", 85)
      ]
```

### Exercise 2

We're going to write a program to figure out how much money people
have after a number of transactions. Fill in the implementation of
`addMoney`:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.State

addMoney :: (String, Int) -> State (HashMap String Int) ()
addMoney = _

main :: IO ()
main =
    print $ execState (mapM_ addMoney transactions) HashMap.empty
  where
    transactions :: [(String, Int)]
    transactions =
      [ ("Alice", 5)
      , ("Bob", 12)
      , ("Alice", 20)
      , ("Charles", 3)
      , ("Bob", -7)
      ]
```

The result should be:

```
fromList [("Bob",5),("Alice",25),("Charles",3)]
```

### Exercise 3

The final output from the previous program is kind of ugly. Instead, I want it to say:

```
Alice: 25
Bob: 5
Charles: 3
```

Modify the program above to get that result.

BONUS: If you really want to experience real-world Haskell code, go
for better performance and use this module:
https://www.stackage.org/haddock/lts-12.21/bytestring-0.10.8.1/Data-ByteString-Builder.html.

### Exercise 4

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (sort)

type Name = String
type StudentId = Int
type Score = Double

students :: HashMap Name StudentId
students = HashMap.fromList
  [ ("Alice", 1)
  , ("Bob", 2)
  , ("Charlie", 3)
  ]

scores :: HashMap Name Score
scores = HashMap.singleton "Bob" 90.4

noTestScore :: [Name]
noTestScore = _

main :: IO ()
main = do
  putStrLn "The following students have not taken the test"
  mapM_ putStrLn $ sort noTestScore
```

Implement `noTestScore` such that the output is:

```
The following students have not taken the test
Alice
Charlie
```

NOTE: hard-coding `["Alice", "Charlie"]` is cheating! :)

### Exercise 5

I want to drop the bottom 20% of test scores. Fill in the helper function.

__NOTE__ I'm switching this exercise from lts-12.21 to lts-12.21. There's
a new helper library function available that makes this much more
straightforward to implement. Browse the docs at:

https://www.stackage.org/haddock/lts-12.21/containers-0.5.10.2/Data-Set.html

You'll also want to use the `div` function, which does integration
division.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import Data.Set (Set)
import qualified Data.Set as Set

scores :: Set Int
scores = Set.fromList [1..10]

dropBottom20Percent :: Ord a => Set a -> Set a
dropBottom20Percent = _

main :: IO ()
main = print $ dropBottom20Percent scores
```

CHALLENGE: Do it for a `HashSet` instead. Why is this different?

## Homework review

### Exercise 1

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

printScore :: Map String Int -> String -> IO ()
printScore scores name =
  case Map.lookup name scores of
    Just score -> putStrLn $ concat
      [ "Score for "
      , name
      , ": "
      , show score
      ]
    Nothing -> putStrLn $ "No score for " ++ name

main :: IO ()
main =
    mapM_ (printScore scores) ["Alice", "Bob", "David"]
  where
    scores :: Map String Int
    scores = Map.fromList
      [ ("Alice", 95)
      , ("Bob", 90)
      , ("Charlies", 85)
      ]
```

### Exercise 2

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.State

addMoney :: (String, Int) -> State (HashMap String Int) ()
addMoney (name, amt) = modify $ HashMap.insertWith (+) name amt

main :: IO ()
main =
    print $ execState (mapM_ addMoney transactions) HashMap.empty
  where
    transactions :: [(String, Int)]
    transactions =
      [ ("Alice", 5)
      , ("Bob", 12)
      , ("Alice", 20)
      , ("Charles", 3)
      , ("Bob", -7)
      ]
```

### Exercise 3

The first thing to note is that the easiest way to sort the people is
to switch from a `HashMap` to a `Map`. A simple find-replace on the
file will work for this.

There are a few different solutions here. Perhaps the most obvious looks like this:

```haskell
import Data.Foldable (for_)

for_ (Map.toList $ execState (mapM_ addMoney transactions) Map.empty)
  $ \(name, total) -> putStrLn $ name ++ ": " ++ show total
```

However, this is arguably _bad_: we're doing more inside `IO` than
really necessary. Instead, we should stick to pure code as much as
possible, and instead build up a `String` value:

```haskell
    putStr $ mapToString $ execState (mapM_ addMoney transactions) Map.empty
  where
    transactions :: [(String, Int)]
    transactions = ...

    pairToString :: (String, Int) -> String
    pairToString (name, total) = name ++ ": " ++ show total

    pairsToString :: [(String, Int)] -> String
    pairsToString = unlines . map pairToString

    mapToString :: Map String Int -> String
    mapToString = pairsToString . Map.toList
```

This reduces the surface area of code that can do dangerous things,
and makes it easier to test our pure functions. One downside of this
is that string concatenation isn't particularly efficient. Using a
bytestring builder approach is even better.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.State
import           Data.ByteString.Builder (Builder, hPutBuilder, intDec)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8Builder)
import           System.IO               (stdout)

addMoney :: (Text, Int) -> State (Map Text Int) ()
addMoney (name, amt) = modify $ Map.insertWith (+) name amt

main :: IO ()
main =
    hPutBuilder stdout $
    mapToBuilder $ execState (mapM_ addMoney transactions) Map.empty
  where
    transactions :: [(Text, Int)]
    transactions =
      [ ("Alice", 5)
      , ("Bob", 12)
      , ("Alice", 20)
      , ("Charles", 3)
      , ("Bob", -7)
      ]

    pairToBuilder :: (Text, Int) -> Builder
    pairToBuilder (name, total) =
      encodeUtf8Builder name <> ": " <> intDec total <> "\n"

    pairsToBuilder :: [(Text, Int)] -> Builder
    pairsToBuilder = foldMap pairToBuilder

    mapToBuilder :: Map Text Int -> Builder
    mapToBuilder = pairsToBuilder . Map.toList
```

This is more involved than the way most people would write this
solution, but provides great performance. Note that it assumes your
output will be UTF8 encoded.

### Exercise 4

```haskell
noTestScore :: [Name]
noTestScore = HashMap.keys $ students `HashMap.difference` scores
```

Also: we can bypass the `sort` if we move over to a `Map` instead.

### Exercise 5

```haskell
dropBottom20Percent :: Ord a => Set a -> Set a
dropBottom20Percent s = Set.drop (Set.size s `div` 5) s
```

## Section exercise

See beginning of <string-types.md>

* Try to use `Builder`s
* In real world code: use proper CSV and HTML libraries
* Bonus for after the course: rewrite with csv-conduit and lucid
