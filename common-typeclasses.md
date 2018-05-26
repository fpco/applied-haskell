# Common Typeclasses

Section exercises:

* Write the `foldMapM` helper function
* Implement the `Validation` `Applicative`
    * Why isn't it a `Monad`?

## Overview

* Not going to cover these in depth
* Great resource on this:
  [typeclassopedia](https://wiki.haskell.org/Typeclassopedia)

## Hysterical raisins

* `Applicative` wasn't a superclass of `Monad` in the past
* `Semigroup` wasn't a superclass of `Monoid` in the past
* Some unnecessary functions still lying around
* Sometimes functions aren't as general as we wish

## Functor

* "Mappable"
* Provides `fmap :: (a -> b) -> (f a -> f b)`
* Laws
    * `fmap id == id`
    * `fmap (g . h) == fmap g . fmap h`
* Cool fact: only one possible valid instance per type
* Can be derived automatically
* Covariant functor (contravariant also exists)
* See: <https://www.fpcomplete.com/blog/2016/11/covariance-contravariance>

## Applicative

Provides:

```haskell
pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b
```

Compare:

```haskell
fmap  ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```

Also note that you can define `fmap` using `Applicative`

```haskell
fmap g x = pure g <*> x
```

Laws:

* `pure id <*> x == x`
* `pure f <*> pure x == pure (f x)`
* `u <*> pure y == pure ($ y) <*> u`
* `u <*> (v <*> w) = pure (.) <*> u <*> v <*> w`

## Monad

Provides:

```haskell
(>>=) :: m a -> (a -> m b) -> m b
```

Or flipped:

```haskell
(=<<) :: (a -> m b) -> m a -> m b
```

Compare:

```haskell
fmap  ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
(=<<) :: (a -> m b) -> m a -> m b
```

Laws:

* `pure a >>= f == f a`
* `m >>= pure == m`
* `m >>= (\x -> f x >>= g) == (m >>= f) >>= g`

And we can define:

```haskell
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
```

And then restate these laws as:

```haskell
f <=< pure == f
pure <=< f == f
(h <=< g) <=< f == h <=< (g <=< f)
```

Which are the same as the category laws:

```haskell
f . id == f
id . f == f
(h . g) . f == h . (g . f)
```

## Semigroup

Defines a binary, associative operator

```haskell
(<>) :: a -> a -> a
```

Law

```haskell
(x <> y) <> z == x <> (y <> z)
```

## Monoid

Adds an identity to `Semigroup`

```haskell
mempty :: a
```

Laws are the same again as `Monad` and categories!

```haskell
x <> mempty == x
mempty <> x == x
(x <> y) <> z == x <> (y <> z)
```

## Foldable

* "I can be turned into a list" but more efficient in some cases.
* `foldMap :: Monoid m => (a -> m) -> f a -> m`
* Can be derived automatically
* No actual laws yet
* Could define a `Vector` that folds left-to-right or right-to-left
* `length` of tuples and other things considered surprising/wrong by
  many

## Traversable

* "Map with effects"
* Generalizes `mapM`
* `traverse` == `mapM`, but works for `Applicative`
* `for` == `forM`, but for `Applicative`

## Exercises

See start of section
