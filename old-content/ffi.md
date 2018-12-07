# Foreign Function Interface

* Call out to C code
* Expose functions to other languages (won't do it here)
* Ugly pointer mangling stuff
* Just getting a taste today

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.Unsafe
import Data.ByteString.Internal (mallocByteString, ByteString (PS))
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word8

foreign import ccall "write"
  c_write :: CInt -> Ptr CChar -> CInt -> IO ()

foreign import ccall "read"
  c_read :: CInt -> Ptr Word8 -> CInt -> IO CInt

main :: IO ()
main = do
  unsafeUseAsCStringLen "Hello FFI World!\n" $ \(ptr, len) ->
    c_write 1 ptr (fromIntegral len)
  let size = 256
  fptr <- mallocByteString size
  len <- withForeignPtr fptr $ \ptr -> c_read 0 ptr $ fromIntegral size
  print $ PS fptr 0 $ fromIntegral len
```

* Problem: copying a file allocates lots of buffers
* Solution: don't do that!

__Exercise__ Write a function that copies standard input to standard
output. Look up `allocaBytes` to help.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Data.Function (fix)
import Control.Monad (when)

foreign import ccall "write"
  c_write :: CInt -> Ptr CChar -> CInt -> IO ()

foreign import ccall "read"
  c_read :: CInt -> Ptr CChar -> CInt -> IO CInt

size :: Int
size = 64 -- use something much bigger in practice

main :: IO ()
main = allocaBytes size $ \ptr -> fix $ \loop -> do
  len <- c_read 0 ptr (fromIntegral size)
  when (len > 0) $ do
    c_write 1 ptr len
    loop
```
