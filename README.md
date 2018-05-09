# Applied Haskell 2018

NOTE: This content is not fully updated for the 2018 training. The
exact order will depend on responses to the questionnaire about
people's preferences. Also: there is more material here than we can
cover in 2 days. Some content will be cut! But this is a decent
collection of content to continue with afterwards.

## Prepare for the training

You should get your system set up in advance so you're ready to get
started.

1. Download and install Stack. Instructions are
   [available online](https://haskell-lang.org/get-started). Make sure
   you have at least version 1.7.
2. We're going to be using LTS 11.X (FIXME!). You may as well install
   an unnecessarily broad number of packages right off the bat:
   `stack build --resolver lts-11.X classy-prelude-yesod lens rio`
3. Make sure you can run the script below successfully. Save it to a
   file ending with `.hs` and then run `stack filename.hs`. On
   non-Windows systems, you can also do `chmod +x filename.hs &&
   ./filename.hs`

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-11.X script
main = putStrLn "Hello World!"
```

Note that the comment on line 2 above is necessary!

My strong recommendation is to take some time to familiarize yourself
with some Haskell concepts before the training. We will be briefly
covering some of these, but you'll follow the material much better if
you take time in advance to familiarize yourself with:

* Haskell syntax. We will be assuming familiarity with all common
  Haskell syntax. Consider
  [Haskell Programming from First Principles](http://haskellbook.com/)
  for this information.
* We'll be assuming knowledge of the `Functor`, `Applicative`, and
  `Monad` typeclasses. The aforementioned book covers these topics. I
  also
  [published a blog post](https://www.snoyman.com/blog/2017/01/functors-applicatives-and-monads)
  which may help. As a self-assessment, I would try to ensure that you
  can pass these exercises:
    * Define `fmap` in terms of `>>=` and `return`
    * Define `fmap` in terms of `<*>` and `pure`
    * Define `>>=` in terms of `Applicative` instance and `join`
    * Define `join` in terms of `>>=`
    * Explain the intuition behind: what can you do with `Monad` and not `Applicative`?
* We'll be covering data structures quite a bit in this
  training. Understanding strictness, laziness, and evaluation in
  Haskell is vital to this. We will touch on this at the beginning of
  the training, but I highly recommend reading my blog post
  [All About Strictness](https://www.fpcomplete.com/blog/2017/09/all-about-strictness)

Feel free to also read the linked material below, which will be used
in the training course itself.

## Intro

* [Overview](overview.md)
* [Synonyms in base](https://haskell-lang.org/tutorial/synonyms)
* [Operator glossary](https://haskell-lang.org/tutorial/operators)
* [Common typeclasses](https://wiki.haskell.org/Typeclassopedia)
* [All About Strictness](https://www.fpcomplete.com/blog/2017/09/all-about-strictness)
* [Data types](data-types.md)

## Data structures

* [Data Structures](data-structures.md)
* [String Types](string-types.md)
* [Vector](vector.md)
* [Containers](containers.md)
* Let's revisit that data structure quiz...

## RIO

* [RIO](rio.md)
* [Lens](lens.md)
* [Monad transformers](monad-transformers.md)
* [Logging](logging.md)
* [External processes via typed-process](https://haskell-lang.org/library/typed-process)

## Concurrency

* [Mutable variables](mutable-variables.md)
* [async](https://haskell-lang.org/library/async)
    * __Exercise__: Write a concurrent program that prints the numbers
      1 to 100 in one thread, pausing one second between number, and
      printing "Hello World" seconds in another thread
* [stm](https://haskell-lang.org/library/stm)
* [Exceptions](exceptions.md)

## Testing

* [hspec](hspec.md) (and some criterion for fun)

## Performance

* [Primitive Haskell](primitive.md)
* [Evaluation order and state tokens](https://wiki.haskell.org/Evaluation_order_and_state_tokens)
* [Profiling](profiling.md)

## Data formats

* [aeson](https://haskell-lang.org/library/aeson)
* [yaml](yaml.md)

## Streaming

* conduit
    * [Slides for later this week](https://www.snoyman.com/reveal/conduit-yesod)
    * [Official tutorial](https://haskell-lang.org/library/conduit)
    * Show of hands: who wants to cover this today?
    * __Exercise__: Take a file and, for each line, print out the
      number of bytes in the line (try using bytestring directly and
      then conduit). Taken from
      [StackOverflow questions](http://stackoverflow.com/questions/42675764/read-large-lines-in-huge-file-without-buffering/42676477#42676477)

## HTTP

* [HTTP clients](https://haskell-lang.org/library/http-client)
    * __Exercise__: Make an HTTP request and print the returned HTML to the console
* [Web services](web-services.md)

## CLIs

* [optparse-applicative](https://haskell-lang.org/library/optparse-applicative)

## Worked examples

* [Length-matched vectors](examples/length-matched-vectors.md)
* [Validation Applicative](examples/validation-applicative.md)
* [Rate limiting using STM](https://github.com/snoyberg/rate-limit)
* [RWST in terms of IORef](https://gist.github.com/snoyberg/7ac111bc873be6a361e452adb5454cb9)
* FIXME add something with a CLI

## Bonus material

* https://www.snoyman.com/reveal/what-makes-haskell-unique
* http://chrisdone.com/posts/haskell-constraint-trick
