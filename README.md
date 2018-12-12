# Applied Haskell 2018

Welcome to the Applied Haskell 2018 course outline. The content below is based
on survey responses from the attendees. Note that there is more material below
than will actually fit into two days! I've organized the material to prioritize
the topics with the highest interest level. I'm leaving this page over-filled
with content so that you can continue after the course finishes if desired.

For sharing code and text chat, please use:

https://gitter.im/applied-haskell/2018-12

## Prepare for the training

You should get your system set up in advance so you're ready to get
started.

1. Download and install Stack. Instructions are
   [available online](https://haskell-lang.org/get-started). Make sure
   you have at least version 1.7.
2. We're going to be using LTS 12.21). You may as well install
   an unnecessarily broad number of packages right off the bat:
   `stack build --resolver lts-12.21 classy-prelude-yesod lens rio yesod-test foldl microlens-platform wai-conduit hspec`

     * You may also find it convenient to run `stack config set resolver lts-12.21`
       from outside of a project to set your global resolver to match.

3. Make sure you can run the script below successfully. Save it to a
   file ending with `.hs` and then run `stack filename.hs`. On
   non-Windows systems, you can also do `chmod +x filename.hs &&
   ./filename.hs`

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
main = putStrLn "Hello World!"
```

Note that the comment on line 2 above is necessary!

## Pre-reading

__tl;dr__: Make you you understand monads, and read [All About
Strictness](https://www.fpcomplete.com/blog/2017/09/all-about-strictness).

As mentioned above, we won't be able to cover everything we'd like in just two
days. Some material will be taken as a prerequisite. For other topics, you will
likely find it easier to familiarize yourself with the concepts before we drill
in on them in the training.

Based on the responses, it seems that everyone attending is at least fairly
solid in Haskell topics up to and including `Functor`/`Applicative`/`Monad`. If
you'd like to test yourself on this, try out the following exercises:

* Define `fmap` in terms of `>>=` and `return`
* Define `fmap` in terms of `<*>` and `pure`
* Define `>>=` in terms of `Applicative` instance and `join`
* Define `join` in terms of `>>=`
* Explain the intuition behind: what can you do with `Monad` and not `Applicative`?

If you'd like to brush up on these topics at all, I [published a blog
post](https://www.snoyman.com/blog/2017/01/functors-applicatives-and-monads)
which is a relatively short read-through. More generally, if you'd like to
brush up on any aspects of Haskell syntax or other basics, I recommend [Haskell
Programming from First Principles](http://haskellbook.com/).

From past experience, understanding strictness, laziness, and evaluation in
Haskell can be a sticking point in understanding the data structure content of
this course (which is a significant basis for the rest of the topics we'll
cover). We'll be covering it in the course itself, but I highly recommend
reading my blog post [All About
Strictness](https://www.fpcomplete.com/blog/2017/09/all-about-strictness).

Below is the actual content for the course itself. Feel free to read through
any of this material in advance of the course as well, though it's less crucial
than the material above.

## Intro

Goal: get on the same page about basics of Haskell, some terminology
we can use, and understand Haskell's evaluation model.

* [Overview](overview.md)
* [Haskell 101](haskell101.md)
* [Data types](data-types.md)
* [Common typeclasses](common-typeclasses.md)
* [All About Strictness](all-about-strictness.md)

## Data structures

Goal: get comfortable with the most common data structures in Haskell.

* [Data Structures](data-structures.md)
* [String Types](string-types.md)
* [Containers](containers.md)
* [Vector](vector.md)
* Let's revisit that data structure quiz...

## RIO

Goal: understand monad transformers, how RIO bypasses most of their
usages, and other reasons to use RIO in general.

* [Monad transformers](monad-transformers.md)
* [RIO](rio.md)

## Concurrency

Write concurrent code safely and easily, avoiding data races almost
automatically.

* [Mutable variables](mutable-variables.md)
* [async](async.md)
* [Exceptions](exceptions.md)

## Testing

"If it compiles it works" is a lie, don't believe it.

* [hspec](hspec.md) (and some gauge benchmarking for fun)

## Performance

Deeper understanding of Haskell performance, and how to improve it.

* [Primitive Haskell](primitive.md)
* [Profiling](profiling.md)

## Configuration

* [yaml](yaml.md)

## Streaming

* [conduit](conduit.md)

## HTTP

* [Web services](web-services.md)
* [HTTP clients](http-client.md)

## Worked examples

* [Length-matched vectors](examples/length-matched-vectors.md)
* [Rate limiting using STM](https://github.com/snoyberg/rate-limit)
* [RWST in terms of IORef](https://gist.github.com/snoyberg/7ac111bc873be6a361e452adb5454cb9)

## Bonus material

* https://www.snoyman.com/reveal/what-makes-haskell-unique
* http://chrisdone.com/posts/haskell-constraint-trick
* [optparse-applicative](https://haskell-lang.org/library/optparse-applicative) for CLIs
