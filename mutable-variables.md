# Mutable variables

Section exercise: write your own `TMVar` implementation.

Use cases:

* Communication among threads
* Let values survive an exception in a `StateT`
* Ugly hacks
* Inherently mutable algorithms (usually for performance)

## IORef

Basic usage:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-11.10 script --optimize
import Data.IORef
import Control.Concurrent.Async (mapConcurrently_)

main :: IO ()
main = do
  ref <- newIORef (0 :: Int)
  modifyIORef ref (+ 1)
  readIORef ref >>= print
  writeIORef ref 2
  readIORef ref >>= print

  -- race condition
  let raceIncr = modifyIORef' ref (+ 1)
  writeIORef ref 0
  mapConcurrently_ id $ replicate 10000 raceIncr
  readIORef ref >>= print

  -- no race condition
  let noRaceIncr = atomicModifyIORef' ref (\x -> (x + 1, ()))
  writeIORef ref 0
  mapConcurrently_ id $ replicate 10000 noRaceIncr
  readIORef ref >>= print
```

To trigger the race condition, run like this:

```
stack --resolver lts-11.10 exec -- ghc -O2 -threaded -with-rtsopts=-N foo.hs && ./foo
```

### Survive exceptions

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-11.10 script
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.IORef
import UnliftIO (MonadUnliftIO, SomeException, tryAny)
import Control.Monad.Reader
import Control.Monad.State.Class
import System.Random (randomRIO)

newtype StateRefT s m a = StateRefT (ReaderT (IORef s) m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance MonadIO m => MonadState s (StateRefT s m) where
  get = StateRefT $ ReaderT $ liftIO . readIORef
  put x = StateRefT $ ReaderT $ \ref -> liftIO $ writeIORef ref $! x

runStateRefT
  :: MonadUnliftIO m
  => StateRefT s m a
  -> s
  -> m (s, Either SomeException a)
runStateRefT (StateRefT (ReaderT f)) s = do
  ref <- liftIO $ newIORef s
  ea <- tryAny $ f ref
  s' <- liftIO $ readIORef ref
  return (s', ea)

main :: IO ()
main = runStateRefT inner 0 >>= print

inner :: StateRefT Int IO ()
inner =
    replicateM_ 10000 go
  where
    go = do
      res <- liftIO $ randomRIO (1, 100)
      if res == (100 :: Int)
        then error "Got 100"
        else modify (+ 1)
```

### Memory usage

Let's calculate fibs (ugh).

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-11.10 script
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Monad
import Data.IORef

main :: IO ()
main = do
  fib1 <- newIORef (0 :: Int)
  fib2 <- newIORef (1 :: Int)

  -- we're gonna overflow, just ignore that
  replicateM_ 1000000 $ do
    x <- readIORef fib1
    y <- readIORef fib2
    writeIORef fib1 y
    writeIORef fib2 $! x + y

  readIORef fib2 >>= print
```

Run it with:

```
stack exec -- ghc foo.hs -O2 && ./foo +RTS -s
```

* `44,384 bytes maximum residency (1 sample(s))` yay!
* However...
* `16,051,920 bytes allocated in the heap`
* Linearly increases too

Problem: `IORef` always stores heap objects, resulting in GC pressure
and pointer indirection. Mutable vectors to the rescue!

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-11.10 script
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as V

main :: IO ()
main = do
  fib1 <- V.replicate 1 (0 :: Int)
  fib2 <- V.replicate 1 (1 :: Int)

  -- we're gonna overflow, just ignore that
  replicateM_ 1000000 $ do
    x <- V.unsafeRead fib1 0
    y <- V.unsafeRead fib2 0
    V.unsafeWrite fib1 0 y
    V.unsafeWrite fib2 0 $! x + y

  V.unsafeRead fib2 0 >>= print
```

* Much better: `51,936 bytes allocated in the heap`
* But that interface sucks
* rio to the rescue!

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-11.10 script
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
import RIO
import Prelude (print)

main :: IO ()
main = do
  fib1 <- newURef (0 :: Int)
  fib2 <- newURef 1

  -- we're gonna overflow, just ignore that
  replicateM_ 1000000 $ do
    x <- readURef fib1
    y <- readURef fib2
    writeURef fib1 y
    writeURef fib2 (x + y)

  readURef fib2 >>= print
```

* Explicit type signatures needed
* Oh yeah, `$!` isn't needed any more
* No `atomic` operations on unboxed arrays

### Concurrency

* `atomicModifyIORef` works well for a single variable
* Cannot lock
* If you got two variables, you're gonna have a bad time

Onward and downward!

## MVar

* Empty or full
* `take` and `put` block if var is empty or full, respectively
* Useful combinators like `modifyMVar`
* We can perform I/O while holding the variable
* Not great for multiple variables, we'll see STM next

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-11.10 script
{-# LANGUAGE NoImplicitPrelude #-}
import RIO
import Prelude (print)
import System.Random (randomRIO)

main :: IO ()
main = do
    var <- newMVar (0 :: Int)
    replicateConcurrently_ 1000 (inner var)
    takeMVar var >>= print
  where
    inner var = modifyMVar_ var $ \val -> do
      -- I'm the only thread currently running. I could play around
      -- with some shared resource like a file
      x <- randomRIO (1, 10)
      return $! val + x
```

Also worth noting: `tryPutMVar` and `tryTakeMVar`.

### Baton

Common pattern: send a notification between threads.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-11.10 script
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import RIO
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Prelude (putStrLn, getLine)

app :: Application
app _req send = send $ responseBuilder status200 [] "Hello World"

main :: IO ()
main = do
    baton <- newEmptyMVar
    race_ (warp baton) (prompt baton)
  where
    warp baton = runSettings
      (setBeforeMainLoop (putMVar baton ()) defaultSettings)
      app
    prompt baton = do
      putStrLn "Waiting for Warp to be ready..."
      takeMVar baton
      putStrLn "Warp is now ready, type 'quit' to exit"
      fix $ \loop -> do
        line <- getLine
        if line == "quit"
          then putStrLn "Goodbye!"
          else putStrLn "I didn't get that, try again" >> loop
```

## Software Transactional Memory

* Atomic transactions
* Trivially handles multiple variables
* No side effects allowed
* Lots of helper data structures
* Recommendation: make this your default for concurrent apps

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-11.10 script
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import RIO
import Say

main :: IO ()
main = do
  seller    <- newTVarIO (0 :: Int)
  buyer     <- newTVarIO (100 :: Int)
  purchases <- newTVarIO (0 :: Int)
  taxes     <- newTVarIO (0 :: Int)
  let makePurchase = join $ atomically $ do
        buyer' <- readTVar buyer
        if buyer' < 10
          then return $ say "Not enough money to make purchase"
          else do
            modifyTVar' buyer (subtract 10)
            modifyTVar' seller (+ 9)
            modifyTVar' taxes (+ 1)
            modifyTVar' purchases (+ 1)
            return $ say "Purchase successful"
  replicateConcurrently_  20 makePurchase
```

Much more to STM: <https://haskell-lang.org/library/stm> FIXME copy it over

## Exercises

* Rewrite Warp example with STM
    * Use a `TMVar` baton
    * Use `TVar` and `check`
* Implement your own `TMVar`
