# String types

Exercise covering this section and the next section (containers):
convert the following CSV-like data:

```
Alice,Los Angeles,California
Bob,New York, New York
Charlie,San Francisco,California
David,Portland,Oregon
Edward,Los Angeles,California
Frank,New York,New York
```

Into this HTML (spacing irrelevant)

```html
<ul>
  <li>California
    <dl>
      <dt>Los Angeles</dt>
      <dd>2</dd>
      <dt>San Francisco</dt>
      <dd>1</dd>
    </dl>
  </li>
  <li>New York
    <dl>
      <dt>New York</dt>
      <dd>2</dd>
    </dt>
  </li>
  <li>Oregon
    <dl>
      <dt>Portland</dt>
      <dd>1</dd>
    </dl>
  </li>
</ul>
```

## Types

* Strings (== list of `Char`s)
* `ByteString` (strict and lazy)
* `Text` (strict and lazy)

## Quiz

* Why do we need `ByteString` and `Text`? Can't we just use `[Word8]`
  and `[Char]`?
* OK, fine, why not use `Vector Word8` and `Vector Char`?
    * Hint: how much memory would `"Hello" :: Vector Char` take?

## Quiz 2: Pick the String type

* Complete works of Shakespeare
* PNG-formatted image
* HTTP headers
* Contents of an HTML file
* Contents of a JSON file

## Builders

What's wrong with this code?

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B8
import Data.Monoid ((<>))

main = B8.putStrLn
    $ "Hello "
   <> "there "
   <> "world, "
   <> "happy "
   <> "to "
   <> "make "
   <> "your "
   <> "acquaintence"
```

* How many buffers are allocated?
* What's the runtime of this?
* Builders: give instructions for filling a buffer, only copy once
  into the final destination
* Easy monoidal API, consider builders a special case of the
  `ByteString` and `Text` data types

![Y u no vector builder?](https://i.imgflip.com/1nuiyg.jpg)

## Strict vs lazy

* Lazy = list of strict chunks
* Reading lazily == lazy I/O == bad (we'll talk about streaming later)
* Writing lazy structure is fine
* For really large values, lazy chunks can be a better memory
  arrangement
* General rule: stick to the strict variants, revert to lazy if you
  have to

## ShortByteString?

* Pinned vs unpinned
* Wait till we discuss vector (storable vs unboxed)
* Can be useful... but maybe just used unboxed `Vector Word8`?
* Not nearly as many convenience functions for `ShortByteString`

## bytestring basics

Perform some I/O

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import Data.Monoid ((<>))

main :: IO ()
main = do
  let fp = "somefile.txt"
  B.writeFile fp $ "Hello " <> "World!"
  contents <- B.readFile fp
  B.putStr $ B.takeWhile (/= 32) contents <> "\n"
```

__Question__ How are our string literals being treated as
`ByteString`s?

Magic numbers like 32 are ugly, `word8` to the rescue!

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import Data.Monoid ((<>))
import Data.Word8 (_space)

main :: IO ()
main = do
  let fp = "somefile.txt"
  B.writeFile fp $ "Hello " <> "World!"
  contents <- B.readFile fp
  B.putStr $ B.takeWhile (/= _space) contents <> "\n"
```

Or assume ASCII directly.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Monoid ((<>))

main :: IO ()
main = do
  let fp = "somefile.txt"
  B.writeFile fp $ "Hello " <> "World!"
  contents <- B.readFile fp
  B8.putStrLn $ B8.takeWhile (/= ' ') contents
```

Downsides of the `Char8` modules

* Lots of breakage for non-ASCII data
* Less efficient than working on `Word8` (conversion to/from `Char`
  can be costly)
* Can be very convenient for some use cases (e.g., HTTP headers)

### Questions

* Is our code exception safe?
* Is there any laziness in the code?
* Discuss: to `Char8` or not `Char8` for `Handle` I/O

### Printing fibs

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibsBS :: [B.ByteString]
fibsBS = map (B8.pack . show) fibs

main :: IO ()
main = B8.putStr $ B8.unlines $ take 5 fibsBS
```

* Propose alternative implementations
* Discuss: performance and assumptions
* `putStr` vs `putStrLn`

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import System.IO (stdout)
import Data.Monoid ((<>))

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main = BB.hPutBuilder stdout $ foldr
  (\i rest -> BB.intDec i <> "\n" <> rest)
  mempty
  (take 5 fibs)
```

* What does the performance of this look like?
* `foldr` or `foldl`?

## Unicode

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = do
  let bs = "Non Latin characters: שלום"
  B8.putStrLn bs
  print bs
```

Output:

```
Non Latin characters: ????
"Non Latin characters: \233\220\213\221"
```

* Implicit data loss!
* Terminal doesn't like incorrect character encodings

## Laziness and undefined

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import UnliftIO.Exception (pureTry)

main :: IO ()
main = do
    let bomb = ['h', 'e', 'l', 'l', 'o', undefined]
    print $ pureTry $ take 5 bomb
    print $ pureTry $ B.take 5 $ B8.pack bomb
    print $ pureTry $ BL.take 5 $ BL8.pack bomb
```

Guess the output!

```
Just "hello"
Nothing
Nothing
```

Let's try again, a little bit bigger.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import UnliftIO.Exception (pureTry)

main :: IO ()
main = do
    let bomb = concat $ replicate 10000 "hello" ++ [undefined]
    print $ pureTry $ take 5 bomb
    print $ pureTry $ B.take 5 $ B8.pack bomb
    print $ pureTry $ BL.take 5 $ BL8.pack bomb
```

Guess the output this time:

```
Just "hello"
Nothing
Just "hello"
```

## Exercise: file copy

* Copy `source.txt` to `dest.txt`
* Step 1: make it work
* Step 2: make it work for large files
    * Hint: you'll want `withBinaryFile` and `hGetSome`
    * Use Stackage to search for them

----

### Solution 1

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.ByteString as B

main = B.readFile "source.txt" >>= B.writeFile "dest.txt"
```

* Problem: reads entire file into memory

### Solution 2

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.ByteString as B
import System.IO
import Data.Function (fix)
import Control.Monad (unless)
main =
  withBinaryFile "source.txt" ReadMode $ \hIn ->
  withBinaryFile "dest.txt" ReadMode $ \hOut ->
  fix $ \loop -> do
    bs <- B.hGetSome hIn 4096
    unless (B.null bs) $ do
      B.hPut hOut bs
      loop
```

* Problem: allocates lots of buffers
* Advanced: can use lower-level FFI functions with reused buffer

## Exercise

Count how many lines are in a file.

## Exercise

Find the largest byte available on standard input.

## Exercise

Find the length of the longest line in a file.

## Exercise

Write out a file with the line "ABC...Z\n" 1000 times.

## text

(Kinda just like bytestring)

## Example: dumb CSV parser

(Please actually use a CSV library for this kind of thing.)

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import Data.Text (Text)
import qualified Data.Text as T

input :: Text
input =
    "Alice,165cm,30y,15\n\
    \Bob,170cm,35y,-20\n\
    \Charlie,175cm,40y,0\n"

parseRow :: Text -> [Text]
parseRow = T.splitOn ","

main :: IO ()
main = mapM_ print $ map parseRow $ T.lines input
```

Did that feel too easy? Good. Consider quoted fields and embedded
commas:

```
"Adams, Adams",165cm,30y,15
"Biggs, Bob",170cm,35y,-20
"Carter, Charlie",175cm,40y,0
```

This is _much_ harder to parse with standard text functions, consider
it extra credit.

## Example: Data.Text.Read for silly format

(Please actually use a real parser library for this kind of thing.)

Consider the following format:

```
Alice 165cm 30y 15
Bob 170cm 35y -20
Charlie 175cm 40y 0
```

We want to parse it to a list of `Person` values:

```haskell
data Person = Person
    { name    :: !Text
    , height  :: !Int
    , age     :: !Int
    , balance :: !Int
    }
```

Give it a shot.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (signed, decimal)
import Data.Maybe (mapMaybe)

input :: Text
input =
    "Alice 165cm 30y 15\n\
    \Bob 170cm 35y -20\n\
    \Charlie 175cm 40y 0\n"

data Person = Person
    { name    :: !Text
    , height  :: !Int
    , age     :: !Int
    , balance :: !Int
    }
    deriving Show

parseLine :: Text -> Maybe Person
parseLine t0 = do
    let (name, t1) = T.break (== ' ') t0
    t2 <- T.stripPrefix " " t1
    Right (height, t3) <- Just $ decimal t2
    t4 <- T.stripPrefix "cm " t3
    Right (age, t5) <- Just $ decimal t4
    t6 <- T.stripPrefix "y " t5
    Right (balance, "") <- Just $ signed decimal t6
    Just Person {..}

main :: IO ()
main = mapM_ print $ mapMaybe parseLine $ T.lines input
```

## Internal representation

* Irrelevant: you don't need to know what a Text looks like!
* But since you're curious...
* UTF-16 encoded
* Uses unpinned memory
* `data Text = Text !Array !Int !Int`, payload, offset, length
* By contrast, `ByteString` is:
* `data ByteString = PS !(ForeignPtr Word8) !Int !Int`

You cannot pass a `Text` directly to FFI or directly look at its
internal data (without diving into deep magic). Consider it fully
opaque!

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CChar)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)

foreign import ccall "write"
    c_write :: Int -> Ptr CChar -> Int -> IO ()

main :: IO ()
main = do
    TIO.putStrLn "What is your name?"
    name <- TIO.getLine
    let msg = "Hello, " <> name <> "\n"
        bs = TE.encodeUtf8 msg
    unsafeUseAsCStringLen bs $ \(ptr, len) ->
        c_write stdoutFD ptr len
  where
    stdoutFD = 1
```

## Character encoding

* Use Text for in-memory representation
* Use ByteString for serialization and I/O
* Convert with `encodeUtf8`/`decodeUtf8`
    * Oh, `decodeUtf8` is partial :(

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE

main = do
  let text = "This is some text, with non-Latin chars: שלום"
      bs = TE.encodeUtf8 text
  B.writeFile "content.txt" bs
  bs2 <- B.readFile "content.txt"
  let text2 = TE.decodeUtf8 bs2
  TIO.putStrLn text2
```

Total decoding:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

main = do
  let text = "This is some text, with non-Latin chars: שלום"
      bs = TE.encodeUtf8 text
  B.writeFile "content.txt" bs
  bs2 <- B.readFile "content.txt"
  let text2 = TE.decodeUtf8With TEE.lenientDecode bs2
  TIO.putStrLn text2
```

Use or `decodeUtf8'`, which returns an `Either`.

__Question__ What character encoding did `TIO.putStrLn` use?

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE

main = do
    let text = "This is some text, with non-Latin chars: שלום"
        bs = TE.encodeUtf8 text
    B.writeFile "content.txt" bs
    text2 <- TIO.readFile "content.txt"
    TIO.putStrLn text2
```

* Good luck!
* http://www.snoyman.com/blog/2016/12/beware-of-readfile

## The char encoding debacle

* File I/O: use bytestring
* Standard handles
    * Interacting with user: use text
    * Interacting with pipe: use bytestring
* Also: text I/O is slow, you can use the `say` package

## Exercise

Take a UTF-8 encoded file and generate a UTF-16 encoded file

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE

main :: IO ()
main = do
    bsUtf8 <- B.readFile "utf8.txt"
    let text = TE.decodeUtf8 bsUtf8
        bsUtf16 = TE.encodeUtf16LE text
    B.writeFile "utf16.txt" bsUtf16
```

## Laziness

* Don't read files lazily
* Writing lazy data isn't actually lazy I/O
* We'll get to streaming data/conduit later

## Further reading

https://haskell-lang.org/tutorial/string-types
