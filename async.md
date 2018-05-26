# The async library

Section exercise: Write a helper function which allows you to pass
actions to worker threads, and which properly handles exceptions for
all of these actions.

FIXME: copy material from
* [async](https://haskell-lang.org/library/async)

## Section exercise

Write a helper function which allows you to pass actions to worker
threads, and which properly handles exceptions for all of these
actions. You'll want to use closable queues from `stm-chans`. Imagine
how you'd allow parallelizing a loop that looks like:

```haskell
myLoop = do
  mnext <- getNextItem
  case mnext of
    Nothing -> pure ()
    Just next -> do
      handleItem next -- want to do this in a worker
      myLoop
```

If you just use `async` or `forkIO`:

* You'll get unbounded worker threads created
* You'll incur the overhead of forking for each item
* You won't have any handling of exceptions

Hint: consider a helper function like:

```haskell
type PerformInWorker = ... -- what should this type be?

withWorkers
  :: Int -- ^ worker count
  -> (PerformInWorker -> IO a)
  -> IO a
```

Bonus:

* Generalize to `MonadUnliftIO`
