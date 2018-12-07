# Exceptions

The short version:

* Import `UnliftIO` or `UnliftIO.Exception`
* Do not import `Control.Exception`
* Done

Long version:

* Slides: https://www.snoyman.com/reveal/async-exception-handling
* Blog post: https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell

Further reading:

* [UnliftIO.Exception](https://www.stackage.org/haddock/lts-12.21/unliftio-0.2.7.0/UnliftIO-Exception.html)
* [safe-exceptions package](https://haskell-lang.org/tutorial/exception-safety)
* [Exceptions best practices](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell)
  (highly opinionated)

## Exercises

* Implement `finally` and `onException` using functions from
  `Control.Exception`
