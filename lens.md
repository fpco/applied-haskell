# Lenses

Firstly, what is lens/lenses/optics?

* A really great solution to the "records problem"
* An almost accidental discover
* Ridiculously complicated type level playground
* Collections of lots of different related things
* The first real subtyping solution in Haskell
* A complete language based on Haskell

## The records problem

Imagine a nested data structure:

```haskell
data Address = Address
  { addressCity :: !Text
  , addressStreet :: !Text
  }
data Person = Person
  { personAddress :: !Address
  , personName :: !Text
  }
```

If you have a value `alice :: Person`, and you want to get the
person's city, you can use record accessors as normal functions:

```haskell
getPersonCity :: Person -> Text
getPersonCity = addressCity . personAddress

alice'sCity :: Text
alice'sCity = getPersonCity alice
```

That's pretty elegant. But let's say that you want to change Alice's
city to something else. In a mutable, object-oriented language, you'd
probably expect something like:

```
alice.address.city = "Los Angeles";
```

The first issue in Haskell is that we can't mutate `alice`; we instead
have to return a _new_ `Person` value with the updated city. Type
signature wise, we'd be looking at:

```haskell
setPersonCity :: Text -> Person -> Person
```

That makes sense. Now let's see how we'd implemente this:

```haskell
import Data.Text (Text)

data Address = Address
  { addressCity :: !Text
  , addressStreet :: !Text
  }
data Person = Person
  { personAddress :: !Address
  , personName :: !Text
  }

getPersonCity :: Person -> Text
getPersonCity = addressCity . personAddress

setPersonCity :: Text -> Person -> Person
setPersonCity city person = person
  { personAddress = (personAddress person)
      { addressCity = city
      }
  }
```

Well... that obviously sucks. It only gets worse as the nesting levels
go deeper. Let's look at some ways to make this easier to stomach.

## Modifier functions

Let's see if we can make this slightly less painful with some modifier
functions:

```haskell
modifyAddressCity :: (Text -> Text) -> Address -> Address
modifyAddressCity f address = address
  { addressCity = f (addressCity address)
  }

modifyPersonAddress :: (Address -> Address) -> Person -> Person
modifyPersonAddress f person = person
  { personAddress = f (personAddress person)
  }

modifyPersonCity :: (Text -> Text) -> Person -> Person
modifyPersonCity = modifyPersonAddress . modifyAddressCity

setPersonCity :: Text -> Person -> Person
setPersonCity city = modifyPersonCity (const city)
```

Composing the modifier functions works nicely, and then we can easily
convert a modifier function into a setter function. Writing the
initial modifier functions is tedious, but that's the price of doing
business.

Another downside is that we've totally separated out the getter and
modifier functions. Let's see if we can combine those.

## Old style lenses

If our problem is splitting up the getters and modifiers, let's just
stick them together.

```haskell
data Lens s a = Lens
  { lensGetter :: s -> a
  , lensModify :: (a -> a) -> s -> s
  }
```

Previously we could compose our getters and modifiers with the good
old `.` function composition operator, but now we need something a bit
more specialized:

```haskell
composeLens :: Lens a b -> Lens b c -> Lens a c
composeLens (Lens getter1 modify1) (Lens getter2 modify2) = Lens
  { lensGetter = getter2 . getter1
  , lensModify = modify1 . modify2
  }
```

With that in hand, we can write lenses for an address's city, a
person's address, put them together, and then easily extract a setter:

```haskell
personAddressL :: Lens Person Address
personAddressL = Lens
  { lensGetter = personAddress
  , lensModify = \f person -> person { personAddress = f (personAddress person) }
  }

addressCityL :: Lens Address Text
addressCityL = Lens
  { lensGetter = addressCity
  , lensModify = \f address -> address { addressCity = f (addressCity address) }
  }

personCityL :: Lens Person Text
personCityL = personAddressL `composeLens` addressCityL

setPersonCity :: Text -> Person -> Person
setPersonCity city = lensModify personCityL (const city)
```

This works, but it feels clunky. It also has some performance overhead
we didn't have previously due to the creation of the `Lens`
values. And a more advanced topic we haven't even touched on yet: it
doesn't allow for _polymorphic update_, which deals with changing type
variables (we won't deal with that for now).

## Van Laarhoven lenses

In all honesty, understanding exactly how these next forms of lenses
work isn't strictly necessary. It's built on the same premise as the
previous kinds of lenses, but it's:

* More efficient
* More easily composable
* Generalizes to other cases
* Handles polymorphic updates
* Produces much crazier error messages

Let's start slowly in motivating this. Our first goal is to see if we
can combine our getter and modifier into a single value, without using
a product type. We need to be able to extract both a getter and
modifier from this value, so it has to provide the following:

```haskell
type Lens s a = ?

view :: Lens s a -> s -> a
view = ?

over :: Lens s a -> (a -> a) -> s -> s
over = ?
```

(Ignore the funny names, they're part of `lens`.)

It doesn't seem like those two output types (`s -> a` and `(a -> a) ->
s -> s`) have much in common. But we're going to use a trick to make
them match up. Let's start with the `over` result:

```haskell
(a -> a) -> (s -> s)
```

I'm going to wrap up the results of the two functions inside the
`Identity` functor:

```haskell
newtype Identity a = Identity { runIdentity :: a }
  deriving Functor

type LensModify s a = (a -> Identity a) -> (s -> Identity s)

over :: LensModify s a -> (a -> a) -> s -> s
over lens f s = runIdentity (lens (Identity . f) s)
```

And we can create values for this lens type with:

```haskell
personAddressL :: LensModify Person Address
personAddressL f person = Identity $ person
  { personAddress = runIdentity $ f $ personAddress person
  }
```

Or, if we want to take advantage of the `Functor` instance and not
play with wrapping and unwrapping the `Identity` values, we get:

```haskell
personAddressL :: LensModify Person Address
personAddressL f person =
      (\address -> person { personAddress = address })
  <$> f (personAddress person)
```

Alright, let's switch over to the getter side. This time around, we
want to start with the same basic `(a -> a) -> (s -> s)`, but apply a
_different_ wrapper type to allow us to get a getter function at the
end, `s -> a`. So in other words, we need to be able to convert from
`s -> Wrapper s` to `s -> a`. This may seem impossible at first, but
it turns out that there's a cool trick to make this happen:

```haskell
newtype Const a b = Const { getConst :: a }
  deriving Functor

type LensGetter s a = s -> Const a s

view :: LensGetter s a -> s -> a
view lens s = getConst (lens s)

personAddressL :: LensGetter Person Address
personAddressL person = Const (personAddress person)
```

This is fairly complex. `Const` is a data type that does the same
thing as the `const` function: it holds onto one value and ignores a
second. Here, `Const` is keeping our `Address` value for us and
allowing use to extract it inside `view`. Stare at it a while and it
will make sense, but it's also just a convoluted way to get to our
goal.

Ultimately, our goal is to make `LensGetter` and `LensModify` the same
thing. But right now, they don't look very similar.

```haskell
type LensModify s a = (a -> Identity a) -> (s -> Identity s)
type LensGetter s a = s -> Const a s
```

In order to bring them more inline, we need to redefine `LensGetter` as:

```haskell
type LensGetter s a = (a -> Const a s) -> (s -> Const a s)
```

And it turns out by shuffling around some things just a bit, we can
make this work as well:

```haskell
type LensGetter s a = (a -> Const a a) -> (s -> Const a s)

view :: LensGetter s a -> s -> a
view lens s = getConst (lens Const s)

personAddressL :: LensGetter Person Address
personAddressL f person = Const $ getConst $ f (personAddress person)
```

Again, kind of crazy, but it works. Our wrapper type is now `Const a`,
and we pass in the `Const` data constructor to the `lens`. It all
kinda sorta works. One final tweak on all of this. Previously, we
defined our modify lens using the `Functor` interface so we didn't
need to know about `Identity` at all:

```haskell
personAddressL :: LensModify Person Address
personAddressL f person =
      (\address -> person { personAddress = address })
  <$> f (personAddress person)
```

It turns out that this _exact same function body_ works for defining
`LensGetter`:

```haskell
personAddressL :: LensGetter Person Address
personAddressL f person =
      (\address -> person { personAddress = address })
  <$> f (personAddress person)
```

And now we can finally unify our getter and modify lenses into one:

```haskell
type Lens s a = forall f. Functor f => (a -> f a) -> (s -> f s)

newtype Identity a = Identity { runIdentity :: a }
  deriving Functor

newtype Const a b = Const { getConst :: a }
  deriving Functor

over :: Lens s a -> (a -> a) -> s -> s
over lens f s = runIdentity (lens (Identity . f) s)

view :: Lens s a -> s -> a
view lens s = getConst (lens Const s)

personAddressL :: Lens Person Address
personAddressL f person =
      (\address -> person { personAddress = address })
  <$> f (personAddress person)

getPersonAddress :: Person -> Address
getPersonAddress = view personAddressL

modifyPersonAddress :: (Address -> Address) -> Person -> Person
modifyPersonAddress = over personAddressL

setPersonAddress :: Address -> Person -> Person
setPersonAddress address = modifyPersonAddress (const address)
```

This means that we have a lens if we support _all possible functors_
in that type signature. It turns out, almost as if by magic, that this
allows us to recapture getter and modifier functions (and via
modifier, setter functions).

The formulation is wonky, and very difficult to grasp. Don't worry if
the intuition hasn't kicked in. It turns out that you can use lenses
quite a bit without fully grokking them.

## Composing lenses

First, let's define a helper function for turning a getter and a
setter into a lens:

```haskell
lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens getter setter = \f s -> setter s <$> f (getter s)
```

Then we can more easily define lenses for `person.address` and
`address.city`:

```haskell
personAddressL :: Lens Person Address
personAddressL = lens personAddress (\x y -> x { personAddress = y })

addressCityL :: Lens Address Text
addressCityL = lens addressCity (\x y -> x { addressCity = y })
```

How do we compose them together into the `person.address.city` lens?
If we expand the type signatures a bit it may become obvious:

```haskell
personAddressL :: Functor f => (Address -> f Address) -> (Person -> f Person)
addressCityL :: Functor f => (Text -> f Text) -> (Address -> f Address)
personCityL :: Functor f => (Text -> f Text) -> (Person -> f Person)
```

How would you implement `personCityL`? Well, turns out to be really
easy:

```haskell
personCityL :: Lens Person Text
personCityL = personAddressL.addressCityL
```

This is a great strength of lenses: _they work with normal function
composition_. They also work in what Haskellers would consider
_backwards_ order: the composition seems to flow from left to right
instead of right to left. But on the other hand, this seems to match
up perfectly with what object oriented developers expect, so that's
nice.

## Helper functions and operators

Dealing directly with the `Lens` type is needlessly painful. Instead,
we work through helper functions and operators. You've already seen
`view`, `over`, and `lens`. Let's implement a few more:

```haskell
(^.) :: s -> Lens s a -> a
s ^. lens = view lens s

infixl 8 ^.

(%~) :: Lens s a -> (a -> a) -> s -> s
(%~) = over

infixr 4 %~

reverseCity :: Person -> Person
reverseCity = personAddressL.addressCityL %~ T.reverse

getCity :: Person -> Text
getCity person = person ^. personAddressL.addressCityL

set :: Lens s a -> a -> s -> s
set lens a s = runIdentity $ lens (\_olda -> Identity a) s

setCity :: Text -> Person -> Person
setCity = set (personAddressL.addressCityL)
```

## Polymorphic updates

It turns out that we can make lenses a bit more general. If we look at
the current type:

```haskell
type Lens s a = forall f. Functor f => (a -> f a) -> (s -> f s)
```

It requires that the field originally be of type `a` and ultimately of
type `a`. It also requires that the value overall is of type `s` and
ultimately of type `s`. Let's create a new datatype where this would
be limiting:

```haskell
data Person age = Person
  { personName :: !Text
  , personAge :: !age
  }

aliceInt :: Person Int
aliceInt = Person "Alice" 30

personAgeL :: Lens (Person age) age
personAgeL = lens personAge (\x y -> x { personAge = y })

setAge :: age -> Person oldAge -> Person age
setAge age person = person { personAge = age }

aliceDouble :: Person Double
aliceDouble = setAge 30.5 aliceInt
```

Try as we might, we cannot use `personAgeL` to change the age value
from an `Int` to a `Double`. Its construction requires that the input
and output `age` type variable remain the same. However, with a small
extension to our `Lens` type, we can make this work:

```haskell
type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

-- Our old monomorphic variant
type Lens' s a = Lens s s a a
```

This says that we have some data structure, `s`. Inside `s` is a value
`a`. If you replace that `a` with a `b`, you get out a `t`. Sound
weird? Let's see it in practice:

```haskell
personAgeL :: Lens (Person age1) (Person age2) age1 age2
personAgeL = lens personAge (\x y -> x { personAge = y })

setAge :: age -> Person oldAge -> Person age
setAge = set personAgeL
```

## Getters, setters, folds, traversals...

What we've seen so far is the original motivating case. It turns out
that there are many crazy ways of generalizing a `Lens` further to
represent more usages. This comes by means of various techniques, such
as:

* Using concrete types
* Using a different typeclass constraint from `Functor`
* Replace functions (e.g., `a -> f b`) with profunctors (e.g. `p a (f b)`)

We've already seen examples of the concrete types approach. Let's use
their more standard names now:

```haskell
type ASetter s t a b = (a -> Identity b) -> s -> Identity t
type ASetter' s a = ASetter s s a a
type Getting r s a = (a -> Const r a) -> s -> Const r s
type SimpleGetter s a = forall r. Getting r s a
```

By using different typeclasses, we're able to create a form of
subtyping. For example, every `Applicative` is also a `Functor`. So if
we define a new type like this:

```haskell
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
```

Every `Lens s t a b` is also a `Traversal s t a b`, but the reverse is
not true. We can go even deeper down the rabit hole with:

```haskell
type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s
```

Now we need `f` to be both `Applicative` and `Contravariant`, so that
all `Traversal`s are `Fold`s, but not all `Fold`s are
`Traversal`s. This actually matches up with the related typeclasses
`Foldable` and `Traversable`, where the latter is a subclass of the
former.

_But what does this have to do with field accessors?_ you may
ask. This is what I was implying above with lens being its own
language on top of Haskell: if desired, you can replace a lot of
functionality found elsewhere with lens-centric code. All of these
different types I've mentioned are known as _optics_, and since they
all have roughly the same shape, they compose together very nicely.

## Packages

The `lens` package itself is fully loaded, and provides _lots_ of
helper functions, operators, types, and generality. It also has a
relatively heavy dependency footprint. Many projects instead use
`microlens`, which has a much lighter footprint, but also lacks some
functionality (for example, prisms).

## Template Haskell

If writing those lenses above by hand seems tedious to you, you're not
alone. Many people use macros/code generation/Template Haskell (all
the same thing in Haskell) to automatically generate lenses, and
sometimes typeclasses to generalize them. Examples:

* https://www.stackage.org/haddock/lts-12.21/microlens-th-0.4.1.1/Lens-Micro-TH.html
* https://www.stackage.org/haddock/lts-12.21/lens-4.15.4/Control-Lens-TH.html

## Best practices

The most important decision to be made for a team is _how_ to use
lenses. Best practices are vital. You do not want half the team using
advanced lens features and the other half not understanding them at
all. I can advise on what I consider best practices, but it will
depend a lot on how your team wants to approach things. What I've
standardized on:

* Using the basic `Lens` types and its functions: solid gold, do it,
  don't hold back! The biggest downside is the slightly confusing
  error messages, but you get used to that quickly
* Replacing common library functions (like `lookup`) with their lensy
  counterparts (like `at`): not worth it. You'll make your code
  shorter, but I prefer the standard Haskell idioms to relearning with
  lens.
* Advanced techniques like prisms, folds, traversals: soft avoidance
  on my part. I think usually the standard Haskell techniques are
  better suited, but again you'll be able to code golf more easily
  with the optics. Prisms in particular are, in my experience, a ripe
  source of bugs, where pattern matching can lead to much clearer
  code.

We should base the homework exercises around how deeply into lenses
the team wants to go.

## Exercise 1

Fill out the stubs below to make the test suite pass. Probably goes
without saying, but: use the generated lenses (`address`, `street`,
`age`, etc) wherever possible instead of falling back to records.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Lens.Micro.Platform
import Data.Text (Text)
import Test.Hspec

data Address = Address
  { _street :: !Text
  , _city :: !Text
  }

makeLenses ''Address

data Person = Person
  { _name :: !Text
  , _address :: !Address
  , _age :: !Int
  }

makeLenses ''Person

hollywood :: Text
hollywood = "Hollywood Blvd"

alice :: Person
alice = Person
  { _name = "Alice"
  , _address = Address
      { _street = hollywood
      , _city = "Los Angeles"
      }
  , _age = 30
  }

wilshire :: Text
wilshire = "Wilshire Blvd"

aliceWilshire :: Person
aliceWilshire = _ -- FIXME set Alice's street to Wilshire

getStreet :: Person -> Text
getStreet = _

-- | Increase age by 1
birthday :: Person -> Person
birthday = _

getAge :: Person -> Int
getAge = _

main :: IO ()
main = hspec $ do
  it "lives on Wilshire" $
    _street (_address aliceWilshire) `shouldBe` wilshire
  it "getStreet works" $
    getStreet alice `shouldBe` hollywood
  it "birthday" $
    getAge (birthday alice) `shouldBe` _age alice + 1
```

## Exercise 2

Remove the `{-# LANGUAGE TemplateHaskell #-}` line from the previous
exercise and get the code to compile. You'll need to define your own
lenses to make this work. Use the `lens` helper function, and make
sure to add type signatures to the values you create.

## Exercise 3

Real challenge: now implement those lenses again, but without using
the `lens` helper function. Instead, use `fmap` or `<$>` directly.

## Exercise 4

There are tuple lenses provided, named `_1`, `_2`, and so on, for
modifying components in tuples. Fill in the stub below so that the
test passes:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import Lens.Micro.Platform
import Test.Hspec

main :: IO ()
main = hspec $
  it "fun with tuples" $
    let tupleLens = _
        tuple :: ((Int, Double), (Bool, Char, String))
        tuple = ((1, 2), (True, 'x', "Hello World"))
     in over tupleLens not tuple `shouldBe`
            ((1, 2), (False, 'x', "Hello World"))
```

## Exercise 5

Everything we've done so far has been on _product types_. In these
cases, lenses work perfectly: we know that we have every field
available. However, lenses do not work perfectly on _sum types_, where
values may or may not exist. In these cases, prisms, traversals, and
folds come into play. We're not necessarily going to be using these in
depth, but it's good to be aware of them.

Let's use the `_Left` and `_Right` prisms (which work as traversals
and folds as well). Fill in the expected values for the test suite
below to begin to get an intuition for how the traversal functions
work.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import Lens.Micro.Platform
import Test.Hspec

main :: IO ()
main = hspec $ do
  it "over left on left" $
    let val :: Either Int Double
        val = Left 5
     in over _Left (+ 1) val `shouldBe` _
  it "over left on right" $
    let val :: Either Int Double
        val = Right 5
     in over _Left (+ 1) val `shouldBe` _
  it "set left on left" $
    let val :: Either Int Double
        val = Left 5
     in set _Left 6 val `shouldBe` _
  it "set left on right" $
    let val :: Either Int Double
        val = Right 5
     in set _Left 6 val `shouldBe` _
```

## Exercise 6

Bonus! This one makes more extreme usage of the folds and
traversals. Reimplement some common library functions using
lenses. This will require looking through the docs for microlens or
lens quite a bit.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE RankNTypes #-}
import Lens.Micro.Platform
import Test.Hspec
import Data.Monoid (Endo)

-- | map/fmap
mapLens :: ASetter s t a b -> (a -> b) -> s -> t
mapLens = _

-- | toList
toListLens :: Getting (Endo [a]) s a -> s -> [a]
toListLens = _

-- | catMaybes
catMaybesLens :: [Maybe a] -> [a]
catMaybesLens = _

main :: IO ()
main = hspec $ do
  it "mapLens" $
    mapLens _2 not ((), True) `shouldBe` ((), False)
  it "toListLens" $
    toListLens both ('x', 'y') `shouldBe` "xy"
  it "catMaybesLens" $
    catMaybesLens [Just 'x', Nothing, Just 'y'] `shouldBe` "xy"
```

## Solutions

### Exercise 1

Note that there are many other ways to solve some of these problems.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Lens.Micro.Platform
import Data.Text (Text)
import Test.Hspec

data Address = Address
  { _street :: !Text
  , _city :: !Text
  }

makeLenses ''Address

data Person = Person
  { _name :: !Text
  , _address :: !Address
  , _age :: !Int
  }

makeLenses ''Person

hollywood :: Text
hollywood = "Hollywood Blvd"

alice :: Person
alice = Person
  { _name = "Alice"
  , _address = Address
      { _street = hollywood
      , _city = "Los Angeles"
      }
  , _age = 30
  }

wilshire :: Text
wilshire = "Wilshire Blvd"

aliceWilshire :: Person
aliceWilshire = set (address.street) wilshire alice

getStreet :: Person -> Text
getStreet = view (address.street)
--getStreet = (^. address.street)

-- | Increase age by 1
birthday :: Person -> Person
birthday = over age (+ 1)
--birthday = age %~ (+ 1)

getAge :: Person -> Int
getAge = view age

main :: IO ()
main = hspec $ do
  it "lives on Wilshire" $
    _street (_address aliceWilshire) `shouldBe` wilshire
  it "getStreet works" $
    getStreet alice `shouldBe` hollywood
  it "birthday" $
    getAge (birthday alice) `shouldBe` _age alice + 1
```

### Exercise 2

```haskell
street :: Lens' Address Text
street = lens _street (\x y -> x { _street = y })

address :: Lens' Person Address
address = lens _address (\x y -> x { _address = y })

age :: Lens' Person Int
age = lens _age (\x y -> x { _age = y })
```

### Exercise 3

```haskell
street :: Lens' Address Text
street f address = (\x -> address { _street = x }) <$> f (_street address)

address :: Lens' Person Address
address f person = (\x -> person { _address = x }) <$> f (_address person)

age :: Lens' Person Int
age f person = (\x -> person { _age = x }) <$> f (_age person)
```

### Exercise 4

```haskell
let tupleLens = _2._1
```

### Exercise 5

The most important bit here to notice: using `set _Left` did _not_
change the data constructor from `Right` to `Left`.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import Lens.Micro.Platform
import Test.Hspec

main :: IO ()
main = hspec $ do
  it "over left on left" $
    let val :: Either Int Double
        val = Left 5
     in over _Left (+ 1) val `shouldBe` Left 6
  it "over left on right" $
    let val :: Either Int Double
        val = Right 5
     in over _Left (+ 1) val `shouldBe` Right 5
  it "set left on left" $
    let val :: Either Int Double
        val = Left 5
     in set _Left 6 val `shouldBe` Left 6
  it "set left on right" $
    let val :: Either Int Double
        val = Right 5
     in set _Left 6 val `shouldBe` Right 5
```

### Exercise 6

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE RankNTypes #-}
import Lens.Micro.Platform
import Test.Hspec
import Data.Monoid (Endo)

-- | map/fmap
mapLens :: ASetter s t a b -> (a -> b) -> s -> t
mapLens l f = over l f

-- | toList
toListLens :: Getting (Endo [a]) s a -> s -> [a]
toListLens l s = s ^.. l

-- | catMaybes
catMaybesLens :: [Maybe a] -> [a]
catMaybesLens list = list ^.. each._Just

main :: IO ()
main = hspec $ do
  it "mapLens" $
    mapLens _2 not ((), True) `shouldBe` ((), False)
  it "toListLens" $
    toListLens both ('x', 'y') `shouldBe` "xy"
  it "catMaybesLens" $
    catMaybesLens [Just 'x', Nothing, Just 'y'] `shouldBe` "xy"
```
