# containers

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
-- stack --resolver lts-8.12 script
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
-- stack --resolver lts-8.12 script
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
