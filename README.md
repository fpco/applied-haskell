# Applied Haskell

* [Overview](overview.md)
* [Data Structures](data-structures.md)
* [String Types](string-types.md)
* [Vector](vector.md)
* [Containers](containers.md)
* Let's revisit that data structure quiz...
* [Data types](data-types.md)
* [Primitive Haskell](primitive.md)
* [mono-traversable and classy-prelude](mono-classy.md)
* [Monad transformers](monad-transformers.md)
* [Exceptions](exceptions.md)
* [Mutable variables](mutable-variables.md)
* [async](https://haskell-lang.org/library/async)
    * __Exercise__: Write a concurrent program that prints the numbers
      1 to 100 in one thread, pausing one second between number, and
      printing "Hello World" seconds in another thread
* [Logging](logging.md)
* [External processes via typed-process](https://haskell-lang.org/library/typed-process)
* [hspec](hspec.md) (and some criterion for fun)
* [Performance](performance.md)
* conduit
    * [Slides for later this week](https://www.snoyman.com/reveal/conduit-yesod)
    * [Official tutorial](https://haskell-lang.org/library/conduit)
    * Show of hands: who wants to cover this today?
    * __Exercise__: Take a file and, for each line, print out the
      number of bytes in the line (try using bytestring directly and
      then conduit). Taken from
      [StackOverflow questions](http://stackoverflow.com/questions/42675764/read-large-lines-in-huge-file-without-buffering/42676477#42676477)
* [HTTP clients](https://haskell-lang.org/library/http-client)
    * __Exercise__: Make an HTTP request and print the returned HTML to the console
* [aeson](https://haskell-lang.org/library/aeson)
* [yaml](yaml.md)
* [Web services](web-services.md)
* [FFI](ffi.md)
