# Mutable variables

Use cases:

* Communication among threads
* Let values survive an exception in a `StateT`
* Ugly hacks
* Inherently mutable algorithms

## IORef

Basic usage:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script --optimize
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
stack --resolver lts-8.12 exec -- ghc -O2 -threaded -with-rtsopts=-N foo.hs && ./foo
```

### Survive exceptions

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.IORef
import Control.Exception.Safe
import Control.Monad.Reader
import Control.Monad.State.Class
import System.Random (randomRIO)

newtype StateRefT s m a = StateRefT (ReaderT (IORef s) m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance MonadIO m => MonadState s (StateRefT s m) where
  get = StateRefT $ ReaderT $ liftIO . readIORef
  put x = StateRefT $ ReaderT $ \ref -> liftIO $ writeIORef ref $! x

runStateRefT
  :: (MonadCatch m, MonadIO m)
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

Challenge: can we write this to work with `ST` as well?

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Primitive.MutVar
import Control.Exception.Safe
import Control.Monad.Reader
import Control.Monad.State.Class
import System.Random (randomRIO)
import Control.Monad.Primitive

newtype StateRefT s m a = StateRefT (ReaderT (MutVar (PrimState m) s) m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance PrimMonad m => MonadState s (StateRefT s m) where
  get = StateRefT $ ReaderT readMutVar
  put x = StateRefT $ ReaderT $ \ref -> writeMutVar ref $! x

runStateRefT
  :: (MonadCatch m, PrimMonad m)
  => StateRefT s m a
  -> s
  -> m (s, Either SomeException a)
runStateRefT (StateRefT (ReaderT f)) s = do
  ref <- newMutVar s
  ea <- tryAny $ f ref
  s' <- readMutVar ref
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
-- stack --resolver lts-8.12 script
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
-- stack --resolver lts-8.12 script
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
* mutable-containers to the rescue!

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Monad
import Data.Mutable

main :: IO ()
main = do
  fib1 <- newRef 0 :: IO (IOURef Int)
  fib2 <- newRef 1 :: IO (IOURef Int)

  -- we're gonna overflow, just ignore that
  replicateM_ 1000000 $ do
    x <- readRef fib1
    y <- readRef fib2
    writeRef fib1 y
    writeRef fib2 (x + y)

  readRef fib2 >>= print
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
-- stack --resolver lts-8.12 script
import Control.Concurrent.MVar
import Control.Concurrent.Async (replicateConcurrently_)
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
-- stack --resolver lts-8.12 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Data.Function (fix)

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
-- stack --resolver lts-8.12 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.STM
import Control.Monad
import Control.Concurrent.Async
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

### Work queue

Problem statement:

> We want to spawn a number of worker threads which will each sleep
> for a random period of time, grab an integer off of a shared work
> queue, square it, and put the result back on a result
> queue. Meanwhile, a master thread will fill up the work queue with
> integers, and read and print results.

Stolen from:
<https://www.fpcomplete.com/blog/2016/11/comparative-concurrency-with-haskell>

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_, concurrently_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan (TBMChan, readTBMChan, writeTBMChan, newTBMChan, closeTBMChan)
import System.Random (randomRIO)

workerCount, workloadCount, minDelay, maxDelay :: Int
workerCount = 250
workloadCount = 10000
minDelay = 250000 -- in microseconds, == 0.25 seconds
maxDelay = 750000 --                  == 0.75 seconds

worker :: TBMChan Int
       -> TBMChan (Int, Int, Int)
       -> Int
       -> IO ()
worker requestChan responseChan workerId = do
    let loop = do
            delay <- randomRIO (minDelay, maxDelay)
            threadDelay delay

            mint <- atomically $ readTBMChan requestChan
            case mint of
                Nothing -> return ()
                Just int -> do
                    atomically $
                        writeTBMChan responseChan (workerId, int, int * int)
                    loop
    loop

main :: IO ()
main = do
    -- Create our communication channels. Now the response channel is
    -- also bounded and closable.
    requestChan <- atomically $ newTBMChan (workerCount * 2)
    responseChan <- atomically $ newTBMChan (workerCount * 2)

    -- We're going to have three main threads. Let's define them all
    -- here. Note that we're _defining_ an action to be run, not
    -- running it yet! We'll run them below.
    let
        -- runWorkers is going to run all of the worker threads
        runWorkers = do
            -- mapConcurrently runs each function in a separate thread
            -- with a different argument from the list, and then waits
            -- for them all to finish. If any of them throw an
            -- exception, all of the other threads are killed, and
            -- then the exception is rethrown.
            mapConcurrently_ (worker requestChan responseChan) [1..workerCount]
            -- Workers are all done, so close the response channel
            atomically $ closeTBMChan responseChan

        -- Fill up the request channel, exactly the same as before
        fillRequests = do
            mapM_ (atomically . writeTBMChan requestChan) [1..workloadCount]
            atomically $ closeTBMChan requestChan

        -- Print each result
        printResults = do
            -- Grab a response if available
            mres <- atomically $ readTBMChan responseChan
            case mres of
                -- No response available, so exit
                Nothing -> return ()
                -- We got a response, so...
                Just (workerId, int, square) -> do
                    -- Print it...
                    putStrLn $ concat
                        [ "Worker #"
                        , show workerId
                        , ": square of "
                        , show int
                        , " is "
                        , show square
                        ]
                    -- And loop!
                    printResults

    -- Now that we've defined our actions, we can use concurrently to
    -- run all of them. This works just like mapConcurrently: it forks
    -- a thread for each action and waits for all threads to exit
    -- successfully. If any thread dies with an exception, the other
    -- threads are killed and the exception is rethrown.
    runWorkers `concurrently_` fillRequests `concurrently_` printResults
```

## Exercises

* Concurrent fibs
    * `IORef`
    * `MVar`
    * `TVar`
* Rewrite Warp example with STM
    * Use a `TMVar` baton
    * Use `TVar` and `check`
* Implement your own `TMVar`
