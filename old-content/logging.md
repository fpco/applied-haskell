# Logging

Two kinds of "logging" to discuss:

* Debug logging from pure code
* Standard logging

## Trace

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import Debug.Trace

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x =
  let res = fib (x - 1) + fib (x - 2)
   in trace ("fib " ++ show x ++ " = " ++ show res) res

main :: IO ()
main = print $ fib 5
```

__Question__ How do you think `trace` is implemented?

Other useful tracing functions:

* `traceId :: String -> String`
* `traceShow :: (Show a) => a -> b -> b`
* `traceShowId :: (Show a) => a -> a`
* `traceM :: (Applicative f) => String -> f ()`
* `traceShowM :: (Show a, Applicative f) => a -> f ()`

 (FIXME: trace vs monad-logger)

__Question__ When is it valid to use `trace`?

## monad-logger

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger.CallStack
import Control.Monad.IO.Class
import UnliftIO.Exception
import qualified Data.Text as T

main :: IO ()
main = runStdoutLoggingT $ do
  let fp = "somefile.txt"
  logInfo $ T.pack $ "Writing to file: " ++ fp
  liftIO (writeFile fp "Hey there!") `onException`
    logError "Writing to file failed"
  logInfo $ T.pack $ "Reading from file: " ++ fp
  content <- liftIO (readFile fp) `onException`
    logError "Reading from file failed"
  logDebug $ T.pack $ "Content read: " ++ content
```

Results in

```
[Info] Writing to file: somefile.txt @(main:Main foo.hs:12:3)
[Info] Reading from file: somefile.txt @(main:Main foo.hs:15:3)
[Debug] Content read: Hey there! @(main:Main foo.hs:18:3)
```

## How it works

* `MonadLogger` type class
* `LoggingT` transformer
* Can use any `IO` function
* `runStdoutLoggingT` and `runStderrLoggingT` uses standard formatting

## Embedding logging function

This doesn't compile:

```haskell
-- Does not compile
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger.CallStack
import Control.Concurrent.Async

main :: IO ()
main = runStdoutLoggingT $ do
  concurrently_
    (logInfo "Hello")
    (logInfo "World")
```

Error

```
9:3: error:
    • Couldn't match expected type ‘LoggingT IO ()’
                  with actual type ‘IO ()’
    • In a stmt of a 'do' block:
        concurrently_ (logInfo "Hello") (logInfo "World")
      In the second argument of ‘($)’, namely
        ‘do { concurrently_ (logInfo "Hello") (logInfo "World") }’
      In the expression:
        runStdoutLoggingT
        $ do { concurrently_ (logInfo "Hello") (logInfo "World") }
```

Multiple solutions

### MonadLoggerIO

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger.CallStack
import Control.Monad.IO.Class
import Control.Concurrent.Async

main :: IO ()
main = runStdoutLoggingT $ do
  logFunc <- askLoggerIO
  liftIO $ concurrently_
    (runLoggingT (logInfo "Hello") logFunc)
    (runLoggingT (logInfo "World") logFunc)
```

### monad-unlift

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger.CallStack
import Control.Monad.IO.Unlift
import Control.Concurrent.Async

main :: IO ()
main = runStdoutLoggingT $ withRunInIO $ \run ->
  liftIO $ concurrently_
    (run (logInfo "Hello"))
    (run (logInfo "World"))
```

### lifted-async

```haskell
-- FIXME #!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger.CallStack
import Control.Monad.IO.Unlift
import Control.Monad
import Control.Concurrent.Async.Lifted.Safe

main :: IO ()
main = runStdoutLoggingT $ do
  void $ concurrently
    (logInfo "Hello")
    (logInfo "World")
```

## Implementing a custom MonadLogger

* Useful for flattening out a transformer stack
* Yesod does this for efficiency

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Monad.Logger.CallStack
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import System.Log.FastLogger (fromLogStr)
import Control.Monad.Reader
import qualified Data.Text as T

data Env = Env
  { envName    :: !String
  , envLogFunc :: !(Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  }

newtype App a = App (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO)
instance MonadReader Env App where
  ask = App ask
  local f (App m) = App $ local f m

instance MonadLogger App where
  monadLoggerLog loc src level msg = do
    env <- ask
    liftIO $ envLogFunc env loc src level $ toLogStr msg

runApp :: Env -> App a -> IO a
runApp env (App (ReaderT f)) = f env

main :: IO ()
main = runApp env $ do
    e <- ask
    logInfo $ T.pack $ "Hello, " ++ envName e
    logDebug "Goodbye!"
  where
    env = Env
      { envName = "Michael"
      , envLogFunc = \loc src level msg ->
          B.putStr $ fromLogStr $ defaultLogStr loc src level msg
      }
```

__Exercise__ Add a `MonadLoggerIO` instance

__Exercise__ Extend `App` to be a `StateT` as well

__Exercise__ Now make it a `StateT` using an `IORef`

## Pure implementation

* Running in `IO` is the common case
* But you can make it pure too

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Control.Monad.Logger.CallStack
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import System.Log.FastLogger (fromLogStr)
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.Text as T

data Env = Env
  { envName    :: !String
  , envLogs    :: !([B.ByteString] -> [B.ByteString])
  -- ^ Difference list
  }

newtype App a = App (StateT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO)
instance MonadReader String App where
  ask = App $ fmap envName get
  local f (App m) = App $ do
    env1 <- get
    put env1 { envName = f $ envName env1 }
    res <- m
    put env1
    return res

instance MonadLogger App where
  monadLoggerLog loc src level msg = App $ do
    let log = fromLogStr $ defaultLogStr loc src level $ toLogStr msg
    modify $ \env -> env { envLogs = envLogs env . (log:) }

runApp :: String -> App a -> IO (a, [B.ByteString])
runApp name (App (StateT f)) = do
  (a, env) <- f Env
    { envName = name
    , envLogs = id
    }
  return (a, envLogs env [])

main :: IO ()
main = do
  ((), logs) <- runApp "Michael" $ do
    e <- ask
    logInfo $ T.pack $ "Hello, " ++ e
    logDebug "Goodbye!"
  mapM_ B.putStr logs
```
