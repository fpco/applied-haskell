# Monad Transformers

Monads are a convenient way to to sequence computation with
effects. Different monads can provide different kinds of effects:

* `IO` allows world-changing side effects
* `Identity` is a "fake" monad: it allows no side effects
* `Reader` lets you access some environment value
* `State` mocks a mutable variable
* `Maybe` allows for early exit
* `Either` allows for early exit with a value

This has nothing to do with a monad transformer, just review. Let's
talk about something totally different.

## Folds with early termination

The typical left fold we've seen requires you to consume the entire
list. However, in some cases, we may want to stop computation
early. As a made up example: let's write a `sum` function that adds up
all numbers until the first negative value:

```haskell
{-# LANGUAGE BangPatterns #-}
sumTillNegative :: [Int] -> Int
sumTillNegative =
    go 0
  where
    go !total rest =
      case rest of
        [] -> total
        x:xs
          | x < 0     -> total
          | otherwise -> go (total + x) xs

main :: IO ()
main = print $ sumTillNegative [1, 2, 3, -1, 4]
```

This works, but it violates all of our engineering principles of non
code duplication. If we had to write a `productTillNegative`, the body
would be almost exactly the same. We should instead factor our some
helper function.

```haskell
{-# LANGUAGE BangPatterns #-}
foldTerminate :: (b -> a -> Either b b) -> b -> [a] -> b
foldTerminate f =
    go
  where
    go !accum rest =
      case rest of
        [] -> accum
        x:xs ->
          case f accum x of
            Left accum' -> accum' -- early termination
            Right accum' -> go accum' xs

sumTillNegative :: [Int] -> Int
sumTillNegative =
    foldTerminate go 0
  where
    go total x
      | x < 0 = Left total
      | otherwise = Right (total + x)

main :: IO ()
main = print $ sumTillNegative [1, 2, 3, -1, 4]
```

## Using Either as a monad

Our implementation internally uses the `Either` data type, and does
explicit pattern matching on it. But we can take advantage of
`Either`'s monad instance, using `do`-notation, and come up with
something arguably slicker:

```haskell
foldTerminate :: (b -> a -> Either b b) -> b -> [a] -> b
foldTerminate f accum0 list0 =
    either id id (go accum0 list0)
  where
    go !accum rest = do
      (x, xs) <-
        case rest of
          [] -> Left accum
          x:xs -> Right (x, xs)
      accum' <- f accum x
      go accum' xs
```

We no longer have to explicitly deal with an exit case: binding with a
`Left` value automatically terminates the loop. Cool!

## How about State?

Previously, we saw that you could implement a left fold using a
`State` monad. This was the non-terminating variety of left fold. It
looked like this:

```haskell
foldState :: (b -> a -> b) -> b -> [a] -> b
foldState f accum0 list0 =
    execState (mapM_ go list0) accum0
  where
    go x = modify' (\accum -> f accum x)
```

We've seen a way to clean up a left fold using `State`, and a way to
clean up terminating loop with `Either`. Can we do both at the same
time? Try as we might, we won't be able to come up with a way to do
this elegantly. The two monads simply don't compose nicely together.

## The StateEither monad

We can fix this problem though! Let's define a new monad,
`StateEither`, which combines the functionality of both `State` and
`Either` together. We can define the type pretty easily:

```haskell
newtype StateEither s e a = StateEither
  { runStateEither :: s -> (s, Either e a)
  }
  deriving Functor
```

This says we take an initial state value, and return an updated state
value, plus an `Either` result value. The expected functionality is
that, when the result is `Left`, we stop processing. But when the
result is `Right`, we continue. Let's write our `Applicative` and
`Monad` instances:

```haskell
instance Applicative (StateEither s e) where
  pure a = StateEither (\s -> (s, Right a))
  StateEither ff <*> StateEither fa = StateEither $ \s0 ->
    case ff s0 of
      (s1, Left e) -> (s1, Left e)
      (s1, Right f) ->
        case fa s1 of
          (s2, Left e) -> (s2, Left e)
          (s2, Right a) -> (s2, Right (f a))

instance Monad (StateEither s e) where
  return = pure
  StateEither f >>= g = StateEither $ \s0 ->
    case f s0 of
      (s1, Left e) -> (s1, Left e)
      (s1, Right x) -> runStateEither (g x) s1
```

Plus some helper functions we were using from `State` before:

```haskell
execStateEither :: StateEither s e a -> s -> s
execStateEither m = fst . runStateEither m

modify' :: (s -> Either e s) -> StateEither s e ()
modify' f = StateEither $ \s0 ->
  case f s0 of
    Left e -> (s0, Left e)
    Right !s1 -> (s1, Right ())
```

With all of tha work in place, it becomes almost trivial to write our
terminating fold:

```haskell
foldTerminate :: (b -> a -> Either b b) -> b -> [a] -> b
foldTerminate f accum0 list0 =
    execStateEither (mapM_ go list0) accum0
  where
    go x = modify' (\accum -> f accum x)
```

We've established three things:

* Monads can make it easier to implement some functions
* Composing monads isn't possible
* But manually defining the compositions _is_ possible

Besides the tediousness of it all, this works great. Homework
exercise: go implement all possible combinations of:

* `Reader`
* `State`
* `Either`
* `IO`

Have fun :)

(Just kidding.)

## Reformulating `StateEither`

Let's play a little rewrite game. Remember, Haskell is a pure
language, so you can always substitue expressions. Turns out you can
also play this game at the type level, using type synonyms. Let's
start with our original type, stripped down a bit:

```haskell
newtype StateEither s e a = StateEither (s -> (s, Either e a))
```

Let's also remember the type of `State`:

```haskell
newtype State s a = State (s -> (s, a))
```

If you stare at those a bit, you'll see that they're _almost_
identical, except we replace `a` with `Either e a` in
`StateEither`. In fact, we can get away with this small rewrite:

```haskell
newtype StateEither s e a = StateEither (State s (Either e a))
```

You should convince yourself that this definition is _isomorphic_ to
the previous definition of `StateEither`. Now we're going to
reimplement our previous example, but we're going to get to take a few
shortcuts. Let's start with the data type and the `Applicative`
instance:

```haskell
newtype StateEither s e a = StateEither
  { unStateEither :: State s (Either e a)
  }
  deriving Functor

instance Applicative (StateEither s e) where
  pure a = StateEither $ return $ Right a
  StateEither ff <*> StateEither fa = StateEither $ do
    ef <- ff
    case ef of
      Left e -> return $ Left e
      Right f -> do
        ea <- fa
        case ea of
          Left e -> return $ Left e
          Right a -> return $ Right $ f a
```

Notice how we _never touch the state value_. Instead, we reuse the
underlying `State`'s `Monad` instance via `do`-notation and `return`
to implement our `Applicative` instance. All we worry about here is
implementing the `Either` shortcut logic. Let's see if this translates
into the `Monad` instance as well:

```haskell
instance Monad (StateEither s e) where
  return = pure
  StateEither f >>= g = StateEither $ do
    ex <- f
    case ex of
      Left e -> return $ Left e
      Right x -> unStateEither $ g x
```

Sure enough it does! Finally, we get some help when implementing our
`execStateEither` and `modify'` helper functions:

```haskell
execStateEither :: StateEither s e a -> s -> s
execStateEither (StateEither m) s = execState m s

modify' :: (s -> Either e s) -> StateEither s e ()
modify' f = StateEither $ do
  s0 <- get
  case f s0 of
    Left e -> return $ Left e
    Right s1 -> do
      put $! s1
      return $ Right ()
```

And our program works exactly as it did before. Sweet.

## Just State?

I'll repeat: in our instances above, we never made direct reference to
the fact that we were using the `State` monad in particular. We just
needed _some_ monad instance. And then our `StateEither` thing comes
along and transforms it into something with a bit more power: the
ability to short-circuit. So... we have a monad... and then we
transform it. I wonder what we'll call this thing...

I know! A monad transformer! We just invented something which
transforms an existing monad (`State` for now) with the `Either`
monad's functionality.

Again, let's look at our data type:

```haskell
newtype StateEither s e a = StateEither
  (State s (Either e a))
```

And instead of hardcoding `State` and `s`, let's take a type variable,
called `m`, to represent whatever monad we're transforming:

```haskell
newtype EitherT e m a = EitherT
  m (Either e a)
```

Convince yourself that, if you replace `m` with `State s`, these two
types are isomorphic. We've called this `EitherT` because it's the
either transformer. (NOTE: for hysterical raisins, in the actual
libraries this is called `ExceptT`, which is a terrible name. Sorry
about that.)

We can still keep our special helper function `execStateEither`:

```haskell
execStateEither :: EitherT e (State s) a -> s -> s
execStateEither (EitherT m) s = execState m s
```

We can also implement our `modify'` function:

```haskell
modify' :: (s -> Either e s) -> EitherT e (State s) ()
modify' f = EitherT $ do
  s0 <- get
  case f s0 of
    Left e -> return $ Left e
    Right s1 -> do
      put $! s1
      return $ Right ()
```

__NOTE__ When we get to mtl, we'll see that we didn't actual need to
write this function, but never mind that for now.

And now, besides changing the type name, our `Applicative` and `Monad`
instances are the same as before, thanks to only using the `Monad`
interface of `State`.

```haskell
instance Monad m => Applicative (EitherT e m) where
  pure a = EitherT $ return $ Right a
  EitherT ff <*> EitherT fa = EitherT $ do
    ef <- ff
    case ef of
      Left e -> return $ Left e
      Right f -> do
        ea <- fa
        case ea of
          Left e -> return $ Left e
          Right a -> return $ Right $ f a

instance Monad m => Monad (EitherT e m) where
  return = pure
  EitherT f >>= g = EitherT $ do
    ex <- f
    case ex of
      Left e -> return $ Left e
      Right x -> runEitherT $ g x
```

In `EitherT e m a`, we call the `m` parameter the _base monad_. For
very good reasons we'll get to later, we always make the base monad
type variable (`m`) the second-to-last variable in defining our
type. We consider `EitherT` a transformer which is layered on top of
the base monad.

## Helper functions

Our previous implementation of `modify'` involved explicitly wrapping
things up with the `EitherT` data constructor. That's not a pleasant
way of interacting with transformers. Instead, we'll want to provide
helper functions. There are two things we need to be able to do for
implementing `modify'`:

* Perform actions from the base monad, namely the `State` monad in
  this case. We call this _lifting_ the action.
* Cause a `Left` value to be returned, triggering an early exit.

We can easily write such helper functions:

```haskell
exitEarly :: Monad m => e -> EitherT e m a
exitEarly e = EitherT $ return $ Left e

lift :: Monad m => m a -> EitherT e m a
lift action = EitherT $ fmap Right $ action
```

Then our `modify'` function turns into:

```haskell
modify' :: (s -> Either e s) -> EitherT e (State s) ()
modify' f = do
  s0 <- lift get
  case f s0 of
    Left e -> exitEarly e
    Right s1 -> lift $ put $! s1
```

Which is significantly simpler.

## Generalizing `lift`

As you've probably guessed, we're going to ultimately implement more
transformers than just `EitherT`. Since lifting actions is the basic
operation of all monad transformers, we want an easy way to do this
across _all_ transformers. To make this work, we're going to define a
typeclass, `MonadTrans`, which provides the `lift` method:

```haskell
class MonadTrans t where
  lift :: Monad m => m a -> t m a
instance MonadTrans (EitherT e) where
  -- lift :: Monad m => m a -> EitherT e m a
  lift action = EitherT $ fmap Right $ action
```

Our definition of `lift` for `EitherT` remains unchanged. All we've
done is generalize the type signature by replacing the concrete
`EitherT e` with a type variable `t`. This is also why we always keep
the last type variable the result type, and the second-to-last the
base monad: it allows us to define this helper typeclass.

The `MonadTrans` typeclass is defined in `Control.Monad.Trans.Class`,
in the `transformers` package.

## Generalizing modify'

Obviously the `modify'` function needs to know about the `State`
monad, since it's explicitly using `get` and `put` actions. And
currently, it's explicitly taking advantage of `EitherT` functionality
as well. But let's try to generalize anyway, and get into the "type
astronaut" world that quickly occurs when overusing monad
transformers.

The monad instance of `EitherT` already handles the short-circuit
logic we're building into our `modify'`. We can generalize by, instead
of returning an `Either e s` value from the provided helper function,
letting the helper function simply run a monadic action. Let's see the
implementation I have in mind first:

```haskell
modifyM f = do
  s0 <- lift get
  s1 <- f s0
  lift $ put $! s1
```

Very elegant: we lift our base monad actions, and allow `f` to perform
actions of its own. Now let's look at the crazy type signature:

```haskell
modifyM
  :: (MonadTrans t, Monad (t (State s)))
  => (s -> t (State s) s)
  -> t (State s) ()
```

In order to use the `lift` function, we need to ensure that the `t`
is, in fact, a monad transformer. Therefore, we say `MonadTrans t`. In
order to use `do`-notation, we need to ensure that our transformer on
top of our base monad (specifically `State` here) is a monad, so we
say `Monad (t (State s))`. And then `t (State s)` in the rest of the
signature is simply how we reference our monad.

Then, in our call site, we replace `modify'` with `modifyM`, and
instead of just an `Either` value, we wrap it up into an `EitherT`
value. We'll define a helper function for that wrapping up:

```haskell
liftEither :: Monad m => Either e a -> EitherT e m a
liftEither = EitherT . return
```

And then rewrite `foldTerminate` to:

```haskell
foldTerminate :: (b -> a -> Either b b) -> b -> [a] -> b
foldTerminate f accum0 list0 =
    execStateEither (mapM_ go list0) accum0
  where
    go x = modifyM (\accum -> liftEither $ f accum x)
```

This certainly shows how powerful and general monad transformers can
be. It's also starting to show some cognitive overhead. So let's make
it one step more general.

## mtl style typeclasses

We've established that not only can the `State` monad itself perform
`get` and `put` actions, but any transformer layered on top of it can
do so as well. The monad transformer library, or mtl, has a philosophy
around generalizing this idea using typeclasses. Let's define a
typeclass, called `MonadState`, for monad _stacks_ which can perform
state-like actions:

```haskell
class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()
```

This uses a new language extension we haven't seen before, called
_functional dependencies_. This means that the type of the monad, `m`,
determines the type of the state, `s`. We use this so that type
inference continues to work nicely, and so that we can't define crazy
things like "this monad allows you to get and put both type `A` and
type `B`."

Anyway, defining an instance for `State` itself is trivial:

```haskell
instance MonadState s (State s) where
  get = State.get
  put = State.put
```

But we can also define an instance for `EitherT` over `State`:

```haskell
instance MonadState s (EitherT e (State s)) where
  get = lift State.get
  put = lift . State.put
```

Or, we can be even more general, and define an instance for `EitherT`
over any monad which is, itself, a `MonadState`:

```haskell
instance MonadState s m => MonadState s (EitherT e m) where
  get = lift get
  put = lift . put
```

With this typeclass and these instances in hand, we can now simplify
our `modifyM` function significantly:

```haskell
modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM f = do
  s0 <- get
  s1 <- f s0
  put $! s1
```

Sweet! Also, as you can probably guess, the `MonadState` typeclass is
already defined for us, in `Control.Monad.State.Class` from the `mtl`
library.

## State is a transformer

Well, sort of. The `State` monad we've been working with until now is,
under the surface, defined as:

```haskell
type State s = StateT s Identity
```

By defining all of our concrete, pure monads in terms of transformers
over the `Identity` monad, we get to implement the functionality only
once.

This is also why the `EitherT` transformer is instead called
`ExceptT`. The author of the library was concerned that it would be
confusing that `type State s = StateT s Identity`, `type Reader r =
ReaderT r Identity`, but the same didn't apply for `Either`.

## No IO transformer

Unlike most (if not all) of the other monads we've talked about, `IO`
does not have a transformer variant. It must always be the base monad,
with other capabilities layered on top of it. For example, `ReaderT
AppConfig IO` is a common way to structure an application: you can
perform `IO` actions, and you can get access to some app-wide config
value.

There is an mtl-style typeclass for `IO`, called creatively
`MonadIO`. It's used quite a bit in the ecosystem, and looks like:

```haskell
class Monad m => MonadIO m where
  liftIO :: IO a -> m a
instance MonadIO IO where
  liftIO = id
instance MonadIO m => MonadIO (EitherT e m) where
  liftIO = lift . liftIO
```

You can generalize many `IO`-specific functions to `MonadIO`, e.g.:

```haskell
readFileGeneral :: MonadIO m => FilePath -> m B.ByteString
readFileGeneral = liftIO . B.readFile
```

`MonadIO` is defined in the `transformers` package in
`Control.Monad.IO.Class`.

__WARNING__ Next topic is significantly more advanced.

One thing you _can't_ automatically lift using `MonadIO` is functions
that take an `IO` action as _input_, also known as _contravariant on
`IO`_ or having `IO` in _negative position_. For example:

```haskell
catchAny :: IO a -> (SomeException -> IO a) -> IO a
```

This function cannot be generalized using `MonadIO`. Instead,
something more powerful needs to come into play. This is a more
advanced topic, but an example of this more powerful entity is
`MonadUnliftIO`, which simplified looks like:

```haskell
class MonadIO m => MonadUnliftIO m where
  askRunInIO :: m (m a -> IO a)
```

This says "I'm going to ask for a function which can convert an action
in this monad stack into a simple `IO` action." Then I can use that to
"knock down" the stacked actions to simple `IO` actions. This is why
it's called _unlifting_: it does the opposite of the lift action. A
simple implementation of `IO` is:

```haskell
instance MonadUnliftIO IO where
  askRunInIO = return id
```

Then we can generalize our `catchAny` function:

```haskell
catchAnyGeneral :: MonadUnliftIO m => m a -> (SomeException -> m a) -> m a
catchAnyGeneral action onExc = do
  run <- askRunInIO
  liftIO $ run action `catchAny` \e -> run (onExc e)
```

Two things to point out:

1. Notice how `MonadUnliftIO` has `MonadIO` as a superclass. We can
   build this subclassing hierarchies, just like we do with
   `Functor`/`Applicative`/`Monad`, where we continuously add more
   restrictions and get more power.
2. Try as you might, you won't be able to define an instance of
   `MonadUnliftIO` for `EitherT`, or a (valid) one for `StateT`. It's
   extremely limited in what it allows, by design. For a long
   explanation:
   [slides](https://www.snoyman.com/reveal/monad-transformer-state)
   and [video](https://www.youtube.com/watch?v=KZIN9f9rI34).

`MonadUnliftIO` is defined in the `unliftio-core` package in
`Control.Monad.IO.Unlift`. The sister package `unliftio` provides an
`UnliftIO` module with _lots_ of built in functionality, like
exception handling, concurrency, and STM, all already generalized to
either `MonadIO` or `MonadUnliftIO`.

## Exercises

You'll want to refer to the documentation for transformers and mtl for
these exercises:

* <https://www.stackage.org/lts-12.21/package/transformers-0.5.2.0>
* <https://www.stackage.org/lts-12.21/package/mtl-2.2.1>

### Exercise 1

Define a monad transformer `ReaderT`, such that the following works:

```haskell
-- Does not compile
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE DeriveFunctor #-}
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity

type Reader r = ReaderT r Identity

runReader :: Reader r a -> r -> a
runReader r = runIdentity . runReaderT r

ask :: Monad m => ReaderT r m r
ask = _

main :: IO ()
main = runReaderT main' "Hello World"

main' :: ReaderT String IO ()
main' = do
  lift $ putStrLn "I'm going to tell you a message"
  liftIO $ putStrLn "The message is:"
  message <- ask
  lift $ putStrLn message
```

### Exercise 2

Create a terminating, _monadic_ fold, which allows you to perform
effects while stepping through the list. There are many different ways
to do this, both with and without monad transformers.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

foldTerminateM :: Monad m => (b -> a -> m (Either b b)) -> b -> [a] -> m b
foldTerminateM = _

loudSumPositive :: [Int] -> IO Int
loudSumPositive =
    foldTerminateM go 0
  where
    go total x
      | x < 0 = do
          putStrLn "Found a negative, stopping"
          return $ Left total
      | otherwise = do
          putStrLn "Non-negative, continuing"
          let total' = total + x
          putStrLn $ "New total: " ++ show total'
          return $ Right total'

main :: IO ()
main = do
  res <- loudSumPositive [1, 2, 3, -1, 5]
  putStrLn $ "Result: " ++ show res
```

The output should be:

```
Non-negative, continuing
New total: 1
Non-negative, continuing
New total: 3
Non-negative, continuing
New total: 6
Found a negative, stopping
Result: 6
```

__NOTE__ Don't be surprised if this exercise is difficult to implement
with transformers. It's a tricky problem.

### Exercise 3

The implementation of `ageInYear` below is unpleasant. Use `MaybeT` to
clean it up.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import Control.Monad.Trans.Maybe
import Text.Read (readMaybe)
import System.IO

prompt :: Read a => String -> IO (Maybe a)
prompt question = do
  putStr question
  putStr ": "
  hFlush stdout
  answer <- getLine
  return $ readMaybe answer

ageInYear :: IO (Maybe Int)
ageInYear = do
  mbirthYear <- prompt "Birth year"
  case mbirthYear of
    Nothing -> return Nothing
    Just birthYear -> do
      mfutureYear <- prompt "Future year"
      case mfutureYear of
        Nothing -> return Nothing
        Just futureYear -> return $ Just $ futureYear - birthYear

main :: IO ()
main = do
  mage <- ageInYear
  case mage of
    Nothing -> putStrLn $ "Some problem with input, sorry"
    Just age -> putStrLn $ "In that year, age will be: " ++ show age
```

### Exercise 4

This example ties together the `ReaderT`+`IO` concept with the lenses
we learned last week. Fix up the following program so that it
compiles.

```haskell
-- Does not compile
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import Control.Monad.Reader
import Lens.Micro
import Lens.Micro.Mtl (view) -- hint :)

data LogLevel = Debug | Info
data Verbosity = Quiet | Verbose

logFunction :: Verbosity -> LogLevel -> String -> IO ()
logFunction Quiet Debug _ = return ()
logFunction _ _ str = putStrLn str

class HasVerbosity env where
  verbosityL :: Lens' env Verbosity

logDebug :: HasVerbosity env => String -> ReaderT env IO ()
logDebug msg = do
  verbosity <- _
  logFunction verbosity Debug msg

logInfo :: HasVerbosity env => String -> ReaderT env IO ()
logInfo = _

main :: IO ()
main = do
  putStrLn "===\nQuiet\n===\n"
  _ inner Quiet
  putStrLn "\n\n===\nVerbose\n===\n"
  _ inner Verbose

inner :: _
inner = do
  logDebug "This is debug level output"
  logInfo "This is info level output"
```

This is the core idea behind `RIO`, which you can read more about at
<https://www.fpcomplete.com/blog/2017/07/the-rio-monad>.

### Exercise 5

Implement a properly strict `WriterT`, including a `MonadWriter`
instance, which internally looks like a `StateT`.

## Solutions

### Exercise 1

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE DeriveFunctor #-}
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
  deriving Functor
instance Monad m => Applicative (ReaderT r m) where
  pure x = ReaderT $ \_ -> pure x
  ReaderT ff <*> ReaderT fa = ReaderT $ \r -> ff r <*> fa r
instance Monad m => Monad (ReaderT r m) where
  return = pure
  ReaderT f >>= g = ReaderT $ \r -> f r >>= flip runReaderT r . g
instance MonadTrans (ReaderT r) where
  lift action = ReaderT $ \_ -> action
instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

type Reader r = ReaderT r Identity

runReader :: Reader r a -> r -> a
runReader r = runIdentity . runReaderT r

ask :: Monad m => ReaderT r m r
ask = ReaderT pure

main :: IO ()
main = runReaderT main' "Hello World"

main' :: ReaderT String IO ()
main' = do
  lift $ putStrLn "I'm going to tell you a message"
  liftIO $ putStrLn "The message is:"
  message <- ask
  lift $ putStrLn message
```

### Exercise 2

One solution: use `MaybeT` to terminate early, and keep the
accumulator in a `StateT`:

```haskell
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

foldTerminateM :: Monad m => (b -> a -> m (Either b b)) -> b -> [a] -> m b
foldTerminateM f accum0 list0 =
    execStateT (runMaybeT $ mapM_ go list0) accum0
  where
    go a = do
      accum0 <- get
      res <- lift $ lift $ f accum0 a
      case res of
        Left accum -> do
          put $! accum
          MaybeT $ return Nothing
        Right accum -> put $! accum
```

Another possibility: use `ExceptT` and put the early terminate value
in the `Left` value via `throwError`:

```haskell
foldTerminateM :: Monad m => (b -> a -> m (Either b b)) -> b -> [a] -> m b
foldTerminateM f accum0 list0 =
    fmap (either id id) $ runExceptT $ execStateT (mapM_ go list0) accum0
  where
    go a = do
      accum0 <- get
      res <- lift $ lift $ f accum0 a
      case res of
        Left accum -> throwError accum
        Right accum -> put $! accum
```

Or, of course, just implement it without transformers at all:

```haskell
foldTerminateM :: Monad m => (b -> a -> m (Either b b)) -> b -> [a] -> m b
foldTerminateM f =
    go
  where
    go !accum [] = return accum
    go !accum (a:as) = do
      res <- f accum a
      case res of
        Left accum' -> return accum'
        Right accum' -> go accum' as
```

Moral of the story: transformers don't always make life easier.

### Exercise 3

```haskell
ageInYear :: IO (Maybe Int)
ageInYear = runMaybeT $ do
  birthYear <- MaybeT $ prompt "Birth year"
  futureYear <- MaybeT $ prompt "Future year"
  return $ futureYear - birthYear
```

### Exercise 4

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import Control.Monad.Reader
import Lens.Micro
import Lens.Micro.Mtl

data LogLevel = Debug | Info
data Verbosity = Quiet | Verbose

logFunction :: Verbosity -> LogLevel -> String -> IO ()
logFunction Quiet Debug _ = return ()
logFunction _ _ str = putStrLn str

class HasVerbosity env where
  verbosityL :: Lens' env Verbosity
instance HasVerbosity Verbosity where
  verbosityL = id

logDebug :: HasVerbosity env => String -> ReaderT env IO ()
logDebug msg = do
  verbosity <- view verbosityL
  liftIO $ logFunction verbosity Debug msg

logInfo :: HasVerbosity env => String -> ReaderT env IO ()
logInfo msg = do
  verbosity <- view verbosityL
  liftIO $ logFunction verbosity Info msg

main :: IO ()
main = do
  putStrLn "===\nQuiet\n===\n"
  runReaderT inner Quiet
  putStrLn "\n\n===\nVerbose\n===\n"
  runReaderT inner Verbose

inner :: ReaderT Verbosity IO ()
inner = do
  logDebug "This is debug level output"
  logInfo "This is info level output"
```

### Exercise 5


```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
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

main :: IO ()
main = pure ()
```

## Further monad transformer info (if desired)

* https://www.snoyman.com/reveal/monad-transformer-state
* https://www.fpcomplete.com/blog/2017/07/announcing-new-unliftio-library
* https://www.fpcomplete.com/blog/2017/07/the-rio-monad
