# rio

Section exercise: use `stack new myproject rio` to create a new
project. Then modify it to run `git ls-files -z` in a directory
specified on the command line, and print out all of the `.md` files.

## What is rio

* My recommendation for Haskell best practices
* Collected from real world code
* Solution to major bugs and productivity woes from companies I've helped
* Somewhat opinionated, but less so than other things I've done :)
* Collection of libraries you should use (and that we're covering today)
* Alternative prelude

I recommend `rio` for new projects. There are a few concepts in `rio`
that deserve to be explained.

## Avoid monad transformers

* Captured the idea in my blog post:
  <https://www.fpcomplete.com/blog/2017/06/readert-design-pattern>
* tl;dr: most application-wide effects are captured by `ReaderT` with
  an `IO`
* So: just capture this in a single data type to make type inference,
  error messages, etc, much nicer
* Use monad transformers "in the small," but not "in the large"
* If you need to do error handling within a transformer, you're
  probably doing it wrong
    * Monad transformer state
      [slides](https://www.snoyman.com/reveal/monad-transformer-state)
      [video](https://www.youtube.com/watch?v=KZIN9f9rI34)
* Just avoid all the problems: use `RIO` for your application

## HasFoo pattern

mtl-style typeclasses encourage two things:

* m*n instance complexity
* Lots of layers of transformers

Different approach in rio. Instead of:

```haskell
class MonadLogger m where
  log :: Text -> m ()
```

Use:

```haskell
class HasLogger env where
  getLog :: env -> (Text -> IO ())

log :: HasLogger env => Text -> RIO env ()

-- more general signature, see liftRIO function
log :: (MonadReader env m, MonadIO m, HasLogger env)
    => Text -> m ()

log text = do
  log <- asks getLog
  liftIO $ log text
```

(But we use lenses, see next section)

Advantages:

* No m*n instance problem
* Deal with functionality directly as functions in a data structure

Disadvantages:

* Some boilerplate incurred
* Hard-coding `IO` at the base, not always appropriate

## Lens

* Would like to be able to update a logging function for a subset of the code
* `getLog` doesn't allow that
* We _could_ introduce `setLog :: (Text -> IO ()) -> env -> env`
* But this is exactly what lenses are for!

In `rio`, we use a very specific subset of lens functionality: the
original getter/setter concept. You don't need to understand all of
lenses, folds, traversals, prisms. Here's the boilerplate you probably
need to deal with:

```haskell
type Logger = Text -> IO ()

class HasLogger env where
  loggerL :: Lens' env (Text -> IO ())
instance HasLogger Logger where
  loggerL = id

data App = App { appLogger :: !Logger }
instance HasLogger App where
  loggerL = lens appLogger (\x y -> x { appLogger = y })

log :: HasLogger env => Text -> RIO env ()
log text = do
  logger <- view loggerL
  liftIO $ logger text
```

Not going to cover the details of lens today, but feel free to read
some content I've written previously: <lens.md>. Also, lots of lens
tutorials online.

## Helper modules

* Annoying having to add lots of packages to `package.yaml`
* Newcomers don't know which libraries to rely on
* rio exports `RIO.ByteString`, `RIO.Text`, etc
* Bonus: partial functions are moved to partial modules
* Bonus: functions are lifted with `MonadIO` and `MonadUnliftIO`

## Batteries included prelude

* Reexports many types
* Removes partial functions
* Reexports all of `UnliftIO`

## Stack template

* `stack new projectname rio`
* Options parsing
* Logging, external process running, all set up
* Test suite

## Logging

Built in support for logging. Uses `HasCallstack` to get file source
location, an efficient builder for output, and deals with character
encoding issues.

## Running external processes

* Mostly follows the tutorial for
  [typed-process](https://haskell-lang.org/library/typed-process)
* However, different definition for `proc` to allow for logging how
  long a process runs for

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import RIO
import RIO.Process
import System.Environment
import System.Exit

-- Here comes the boilerplate! Template helps with this.
data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  }
instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

main :: IO ()
main = do
  -- more boilerplate
  lo <- logOptionsHandle stderr True
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          }
     in runRIO app run

run :: RIO App ()
run = do
  args <- liftIO getArgs
  case args of
    [] -> do
      logError "You need to provide a command to run"
      liftIO exitFailure
    x:xs -> proc x xs runProcess_
```

## Exercise

See above
