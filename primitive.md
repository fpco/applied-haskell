# Primitive Haskell

Section exercise: write the `RIO` monad without using the `IO` type at
all.

__NOTE__ Most Haskell programmers will never need to write code that
uses primitive Haskell operations. However, analyzing GHC core is much
easier if you understand this. Plus, it can be fun to understand
this. Consider this unwrapping a complete layer of abstraction that
won't often, if ever, be necessary.

## More primops

We touched on some primops for adding before:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE MagicHash #-}
import GHC.Prim
import GHC.Types

main :: IO ()
main = print $ I# (5# +# 6#)
```

What other goodies do we have?

<https://www.stackage.org/haddock/lts-12.21/ghc-prim-0.5.1.1/GHC-Prim.html>

```haskell
data MutVar# s a
newMutVar# :: a -> State# s -> (#State# s, MutVar# s a#)
readMutVar# :: MutVar# s a -> State# s -> (#State# s, a#)
writeMutVar# :: MutVar# s a -> a -> State# s -> State# s
```

Basis for `IORef` and `STRef`. Two pieces of magic here:

* Unboxed tuples
* The `State#` business

This is how `IO` is implemented internally in GHC:

```haskell
newtype IO   a = IO (State# RealWorld -> (# State# RealWorld, a #))
newtype ST s a = ST (State# s         -> (# State# s,         a #))
```

### Converting IORef to MutVar

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import Data.IORef

main :: IO ()
main = do
  fib1 <- newIORef (0 :: Int)
  fib2 <- newIORef (1 :: Int)
  let loop 0 = readIORef fib1
      loop remaining = do
        x <- readIORef fib1
        y <- readIORef fib2
        writeIORef fib1 y
        writeIORef fib2 $! x + y
        loop (remaining - 1)
  loop 10 >>= print
```

Rewritten with unboxed stuff:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Prim
import GHC.Types

main :: IO ()
main = start >>= print

start :: IO Int
start = IO $ \s0 ->
  -- NOTE: Int# wouldn't work here, ask me why :)
  case newMutVar# (0 :: Int) s0 of
    (# s1, fib1 #) -> case newMutVar# (1 :: Int) s1 of
      (# s2, fib2 #) ->
        let loop 0 s3 = readMutVar# fib1 s3
            loop remaining s3 =
              case readMutVar# fib1 s3 of
                (# s4, x #) -> case readMutVar# fib2 s4 of
                  (# s5, y #) -> case writeMutVar# fib1 y s5 of
                    s6 ->
                      let xy = x + y
                       in xy `seq`
                          case writeMutVar# fib2 xy s6 of
                            s7 -> loop (remaining - 1) s7
         in loop 10 s2
```

__Exercise__ Rewrite `return` and `>>=` for `IO` with unboxed
functions.

__Exercise__ Rewrite `runST`. You'll need
[`runRW#`](https://www.stackage.org/haddock/lts-12.21/ghc-prim-0.5.1.1/GHC-Magic.html#v:runRW-35-)
in `GHC.Magic`.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
import GHC.Prim
import GHC.Types
import GHC.ST
import GHC.Magic

runIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
runIO (IO f) s = f s

returnIO :: a -> IO a
returnIO x = IO $ \s -> (# s, x #)

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO ioa) f = IO $ \s0 ->
  case ioa s0 of
    (# s1, a #) -> runIO (f a) s1

runST :: (forall s. ST s a) -> a
runST (ST f) =
  case runRW# f of
    (# _ignoredState, x #) -> x

main :: IO ()
main = pure ()
```

__Exercise__ Implement a scaled down version of `ST` with a `Monad`
instance and the following signature:

```haskell
newtype ST s a
runST :: (forall s. ST s a) -> a
unsafeToST :: IO a -> ST s a
```

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
import GHC.Prim
import GHC.Magic
import GHC.Types

newtype ST s a = ST (State# s -> (# State# s, a #))
  deriving Functor

instance Applicative (ST s) where
  pure x = ST (\s -> (# s, x #))
  ST f <*> ST x = ST $ \s0 ->
    case f s0 of
      (# s1, f' #) ->
        case x s1 of
          (# s2, x' #) -> (# s2, f' x' #)

instance Monad (ST s) where
  return = pure
  ST x >>= f = ST $ \s0 ->
    case x s0 of
      (# s1, x' #) ->
        case f x' of
          ST f' -> f' s1

runST :: (forall s. ST s a) -> a
runST (ST f) =
  case runRW# f of
    (# _s, x #) -> x

unsafeToST :: IO a -> ST s a
unsafeToST (IO f) = ST (unsafeCoerce# f)

main :: IO ()
main = print $! runST $ do
  unsafeToST $ putStrLn "Enter your name"
  unsafeToST getLine
```

__Question__ Why is $! necessary?

## unsafePerformIO

What do you think `unsafePerformIO`'s implementation looks like? Let's
see:

<https://wiki.haskell.org/Evaluation_order_and_state_tokens>

Total bonus section...

## Exercise

Write the `RIO` monad without using the `IO` type at all.
