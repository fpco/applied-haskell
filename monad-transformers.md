# Monad Transformers

## Warning: WriterT is bad!

* Strict ain't strict:
  <https://mail.haskell.org/pipermail/libraries/2012-October/018599.html>
* Odds are you shouldn't use `WriterT` at all, too easy to leak space
* By extension, `RWST` is a bad idea
* For that matter, lazy `StateT` is usually a mistake as well

## ReaderT

* Pass an environment around implicitly

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Control.Monad.Reader

main :: IO ()
main = runReaderT inner "Michael"

inner :: ReaderT String IO ()
inner = do
  name <- ask
  lift $ putStrLn $ "Name: " ++ name
  local (++ "!") $ do
    name <- ask
    liftIO $ putStrLn $ "Name with energy: " ++ name
```

### MonadTrans and MonadIO

* `lift` moves us up one layer in the monad stack
* `liftIO` moves us all the way to the base `IO` monad
* `liftBase` is a more general version of `liftIO`

### mtl typeclasses

* MonadReader allows you to have a `ReaderT` deeper in the stack
* By the way: usually a bad idea to have a deep monad stack...

### The Has class trick

* Write functions that work with different "environments"
* Good for complicated configs

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Control.Monad.Reader

newtype Name = Name String
newtype Profession = Profession String
data Person = Person !Name !Profession

class HasName a where
  getName :: a -> Name
instance HasName Name where
  getName = id
instance HasName Person where
  getName (Person name _) = name

main :: IO ()
main = do
  runReaderT inner $ Name "Michael"
  runReaderT inner $ Person
    (Name "Michael")
    (Profession "Bug creator")

inner :: (MonadReader env m, HasName env, MonadIO m) => m ()
inner = do
  Name name <- getName <$> ask
  liftIO $ putStrLn $ "Name: " ++ name
```

Wanna get really fancy? Lenses!

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Control.Monad.Reader
import Lens.Micro
import Lens.Micro.Extras

newtype Name = Name String
newtype Profession = Profession String
data Person = Person !Name !Profession

class HasName a where
  name :: Lens' a Name
instance HasName Name where
  name = id
instance HasName Person where
  name = lens
    (\(Person name _) -> name)
    (\(Person _ prof) name -> Person name prof)

main :: IO ()
main = do
  runReaderT inner $ Name "Michael"
  runReaderT inner $ Person
    (Name "Michael")
    (Profession "Bug creator")

inner :: (MonadReader env m, HasName env, MonadIO m) => m ()
inner = do
  Name name <- view name <$> ask
  liftIO $ putStrLn $ "Name: " ++ name
```

### ReaderIO

Let's pull it together with an implementation of a `ReaderIO` type
using primitives. __There is no reason to ever use this!__

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Monad.Reader
import Lens.Micro
import Lens.Micro.Extras
import GHC.Prim
import GHC.Types

newtype ReaderIO env a = ReaderIO
  { unReaderIO :: env
               -> State# RealWorld
               -> (# State# RealWorld, a #)
  }

runReaderIO :: ReaderIO env a -> env -> IO a
runReaderIO (ReaderIO f) env = IO (\s -> f env s)

instance Functor (ReaderIO env) where
  fmap f (ReaderIO g) = ReaderIO $ \env s ->
    case g env s of
      (# s', x #) -> (# s', f x #)
instance Applicative (ReaderIO env) where
  pure x = ReaderIO (\_env s -> (# s, x #))
  ReaderIO f <*> ReaderIO x = ReaderIO $ \env s0 ->
    case f env s0 of
      (# s1, f' #) ->
        case x env s1 of
          (# s2, x' #) -> (# s2, f' x' #)
instance Monad (ReaderIO env) where
  return = pure
  ReaderIO mx >>= f = ReaderIO $ \env s0 ->
    case mx env s0 of
      (# s1, x #) -> unReaderIO (f x) env s1
-- not a real monad transformer, no MonadTrans!
instance MonadIO (ReaderIO env) where
  liftIO (IO f) = ReaderIO (\_env -> f)
instance MonadReader env (ReaderIO env) where
  ask = ReaderIO (\env s -> (# s, env #))
  local f (ReaderIO g) = ReaderIO $ \env s -> g (f env) s

newtype Name = Name String
newtype Profession = Profession String
data Person = Person !Name !Profession

class HasName a where
  name :: Lens' a Name
instance HasName Name where
  name = id
instance HasName Person where
  name = lens
    (\(Person name _) -> name)
    (\(Person _ prof) name -> Person name prof)

main :: IO ()
main = do
  runReaderIO inner $ Name "Michael"
  runReaderIO inner $ Person
    (Name "Michael")
    (Profession "Bug creator")

inner :: (MonadReader env m, HasName env, MonadIO m) => m ()
inner = do
  Name name <- view name <$> ask
  liftIO $ putStrLn $ "Name: " ++ name
```

## PrimMonad/PrimBase

* PrimMonad: things that can lift to a state transformer (IO or ST and
  transformers on top of them)
* PrimBase: things that _are_ a state transformer (IO or ST only)

Example: `ReaderIO` is a `PrimMonad`, but not a `PrimBase`

```haskell
instance PrimMonad (ReaderIO env) where
  type PrimState (ReaderIO env) = RealWorld
  primitive f = ReaderIO (\_env -> f)
```

## MonadBaseControl

Can we generalize this to `MonadIO`?

```haskell
replicateIO_ :: Int -> IO () -> IO ()
```

Answer: no, because `IO` appears in _negative position_. `MonadIO`
only works for things in _positive position_.

To do this, we need `MonadBaseControl`:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.State.Strict

replicateIO :: Int -> IO () -> IO ()
replicateIO = replicateM_

replicateBase :: MonadBaseControl IO m => Int -> m () -> m ()
replicateBase count action =
  liftBaseWith $ \run ->
  replicateIO count (void (run action))

main :: IO ()
main = do
  res <- execStateT (replicateBase 5 inner) (0 :: Int)
  putStrLn $ "Final: " ++ show res
  where
    inner = do
      x <- get
      liftIO $ putStrLn $ "Current value: " ++ show x
      put $! x + 1
```

Unfortunately, output isn't what we'd want:

```
Current value: 0
Current value: 0
Current value: 0
Current value: 0
Current value: 0
Final: 0
```

__Question__ Who can figure out why?

## monad-unlift

Use type system to ensure there's no discarded state.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Unlift

replicateIO :: Int -> IO () -> IO ()
replicateIO = replicateM_

replicateBase :: MonadBaseUnlift IO m => Int -> m () -> m ()
replicateBase count action = do
  run <- askRunBase
  liftBase $ replicateIO count $ run action

main :: IO ()
main = do
  runReaderT
    (replicateBase 5 $ do
      name <- ask
      liftIO $ putStrLn $ "Hello " ++ name)
    "Michael"
```

Unfortunately gives horrible error messages:

```
26:22: error:
    • Couldn't match type ‘constraints-0.9.1:Data.Constraint.Forall.Skolem
                             (Control.Monad.Trans.Unlift.IdenticalBase (StateT Int IO))’
                     with ‘(constraints-0.9.1:Data.Constraint.Forall.Skolem
                              (Control.Monad.Trans.Unlift.IdenticalBase (StateT Int IO)),
                            Int)’
        arising from a use of ‘replicateBase’
    • In the first argument of ‘execStateT’, namely
        ‘(replicateBase 5 inner)’
      In a stmt of a 'do' block:
        res <- execStateT (replicateBase 5 inner) (0 :: Int)
      In the expression:
        do { runReaderT
               (replicateBase 5
                $ do { name <- ask;
                       liftIO $ putStrLn $ "Hello " ++ name })
               "Michael";
             res <- execStateT (replicateBase 5 inner) (0 :: Int);
             putStrLn $ "Final: " ++ show res }
```

## lifted-base and alternatives

* `lifted-base` provides `MonadBaseControl`-lifted versions of
  functions
* For exception handling, use `safe-exceptions` instead (covered
  later)

## Other common transformers

* `StateT`, use strict
* `ExceptT`, I don't like it :)
* `ContT`, "the mother of all monads", pretty tricky to get right

## Exercise

Implement a properly strict `WriterT`, including a `MonadWriter`
instance, which internally looks like a `StateT`.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
import Control.Monad.Writer.Class
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype WriterT w m a = WriterT (w -> m (a, w))
  deriving Functor

instance Monad m => Applicative (WriterT w m) where
  pure x = WriterT $ \w -> pure (x, w)
  WriterT f <*> WriterT x = WriterT $ \w0 -> do
    (f', w1) <- f w0
    (x', w2) <- x w1
    pure (f' x', w2)

instance Monad m => Monad (WriterT w m) where
  return = pure
  WriterT x >>= f = WriterT $ \w0 -> do
    (x', w1) <- x w0
    let WriterT f' = f x'
    f' w1

instance MonadTrans (WriterT w) where
  lift f = WriterT $ \w -> do
    x <- f
    pure (x, w)

instance MonadIO m => MonadIO (WriterT w m) where
  liftIO = lift . liftIO

instance (Monad m, Monoid w) => MonadWriter w (WriterT w m) where
  tell w2 = WriterT $ \w1 -> pure ((), w1 `mappend` w2)
  pass (WriterT f) = WriterT $ \w0 -> do
    ((a, f), w1) <- f w0
    pure (a, f w1)
  listen (WriterT m) = WriterT $ \w0 -> do
    (a, w) <- m mempty
    pure ((a, w), w0 `mappend` w)

runWriterT :: (Monad m, Monoid w) => WriterT w m a -> m (a, w)
runWriterT (WriterT f) = f mempty
```
