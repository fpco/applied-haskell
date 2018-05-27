# Length matched vectors

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module VectorLenMatched
   ( VectorLM
   , LMKey
   , lmKeys
   , unVectorLM
   , lookupVectorLM
   , withVectorLM
   , newVectorLM
   , unknownKey
   ) where

import RIO
import qualified RIO.Vector as V
import RIO.Vector.Unsafe (unsafeIndex)
import Data.Proxy
import Data.Reflection
import Data.Void

newtype LMKey k = LMKey Int

newtype VectorLM k a = VectorLM (Vector a)

unknownKey :: Vector a -> VectorLM () a
unknownKey = VectorLM

unVectorLM :: VectorLM k a -> Vector a
unVectorLM (VectorLM v) = v

lookupVectorLM :: LMKey k -> VectorLM k a -> a
lookupVectorLM (LMKey idx) (VectorLM v) = unsafeIndex v idx

lmKeys :: forall k. Reifies k Int => [LMKey k]
lmKeys =
  case reflect (Proxy :: Proxy k) of
    0 -> []
    i -> map LMKey [0..i - 1]

withVectorLM
  :: forall a r.
     Vector a
  -> (forall k. Reifies k Int => VectorLM k a -> r)
  -> r
withVectorLM v inner = reify (V.length v) $ \p -> inner $ addType p v

addType :: proxy k -> Vector a -> VectorLM k a
addType _ = VectorLM

newVectorLM :: forall k a. Reifies k Int => Vector a -> Maybe (VectorLM k a)
newVectorLM v
  | V.length v == len = Just (VectorLM v)
  | otherwise = Nothing
  where
    len = reflect (Proxy :: Proxy k)
```
