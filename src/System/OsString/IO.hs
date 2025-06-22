{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module System.OsString.IO (hPut, hPutStr, hPutStrLn, putStr, putStrLn) where

-- Constructors needed for coercions

import Data.Array.Byte (ByteArray (..), MutableByteArray (..))
import Data.ByteString.Short (ShortByteString (..))
import Data.Coerce (coerce)
import Data.Word (Word8)
import GHC.Base (IO (..))
import GHC.Exts qualified as Exts
import GHC.ST (ST (..), runST)
import GHC.ST qualified
import System.IO (Handle, stdout)
import System.IO qualified as IO
import System.OsString qualified as OsString
import System.OsString.Internal.Types (OsString (..))

-- Constructors needed for coercions
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.OsString.Internal.Types (WindowsString(..))
#else
import System.OsString.Internal.Types (PosixString(..))
#endif

import Prelude hiding (putStr, putStrLn)

--------------------------------------------------------------
-- Copied from 'primitive' package
--------------------------------------------------------------
primitive :: GHC.ST.STRep s a -> ST s a
primitive = ST

primitive_ :: (Exts.State# (s) -> Exts.State# (s)) -> ST s ()
{-# INLINE primitive_ #-}
primitive_ f =
  primitive
    ( \s# ->
        case f s# of
          s'# -> (# s'#, () #)
    )

copyByteArray
  :: MutableByteArray s
  -- ^ destination array
  -> Int
  -- ^ offset into destination array
  -> ByteArray
  -- ^ source array
  -> Int
  -- ^ offset into source array
  -> Int
  -- ^ number of bytes to copy
  -> ST s ()
{-# INLINE copyByteArray #-}
copyByteArray (MutableByteArray dst#) doff (ByteArray src#) soff sz =
  primitive_ (Exts.copyByteArray# src# (unI# soff) dst# (unI# doff) (unI# sz))

unI# :: Int -> Exts.Int#
unI# (Exts.I# i) = i

sizeofByteArray :: ByteArray -> Int
{-# INLINE sizeofByteArray #-}
sizeofByteArray (ByteArray arr#) = Exts.I# (Exts.sizeofByteArray# arr#)

isByteArrayPinned :: ByteArray -> Bool
{-# INLINE isByteArrayPinned #-}
isByteArrayPinned (ByteArray arr#) = Exts.isTrue# (Exts.isByteArrayPinned# arr#)

newPinnedByteArray :: Int -> ST s (MutableByteArray s)
{-# INLINE newPinnedByteArray #-}
newPinnedByteArray (Exts.I# n#) =
  primitive
    ( \s# -> case Exts.newPinnedByteArray# n# s# of
        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #)
    )

unsafeFreezeByteArray
  :: MutableByteArray s -> ST s ByteArray
{-# INLINE unsafeFreezeByteArray #-}
unsafeFreezeByteArray (MutableByteArray arr#) =
  primitive
    ( \s# -> case Exts.unsafeFreezeByteArray# arr# s# of
        (# s'#, arr'# #) -> (# s'#, ByteArray arr'# #)
    )

byteArrayContents :: ByteArray -> Exts.Ptr Word8
{-# INLINE byteArrayContents #-}
byteArrayContents (ByteArray arr#) = Exts.Ptr (Exts.byteArrayContents# arr#)

--------------------------------------------------------------
-- Copied from 'byteslice' package
--------------------------------------------------------------

touchByteArrayIO :: ByteArray -> IO ()
touchByteArrayIO (ByteArray x) =
  IO (\s -> (# Exts.touch# x s, () #))

{- | Yields a pinned ByteArray whose contents are identical to those
of the original byte sequence. If the @ByteArray@ backing the argument
was already pinned, this simply aliases the argument and does not perform
any copying.
-}
pin :: ByteArray -> ByteArray
pin b =
  if isByteArrayPinned b
    then b
    else
      let len = sizeofByteArray b
       in runST $ do
            dst <- newPinnedByteArray len
            copyByteArray dst 0 b 0 len
            unsafeFreezeByteArray dst

{- | Outputs @OsString@ to the specified @Handle@. This is implemented
with 'System.IO.hPutBuf'.
-}
hPut :: Handle -> OsString -> IO ()
hPut h os = do
  let b1 = pin (coerce os)
  IO.hPutBuf h (byteArrayContents b1) (sizeofByteArray b1)
  touchByteArrayIO b1

-- | A synonym for hPut.
hPutStr :: Handle -> OsString -> IO ()
hPutStr = hPut

-- | Write a @ByteString@ to a handle, appending a newline byte.
hPutStrLn :: Handle -> OsString -> IO ()
hPutStrLn h os = do
  hPut h (OsString.snoc os (OsString.unsafeFromChar '\n'))

-- | Write a @ByteString@ to stdout.
putStr :: OsString -> IO ()
putStr = hPut stdout

-- | Write a @ByteString@ to stdout, appending a newline byte.
putStrLn :: OsString -> IO ()
putStrLn = hPutStrLn stdout
