{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module System.OsString.IO (hPut, hPutStr, hPutStrLn, putStr, putStrLn) where

import Control.Monad.ST (runST)
import Data.Coerce (coerce)
import Data.Primitive (ByteArray (..))
import Data.Primitive qualified as PM
import GHC.Base (IO (..))
import GHC.Exts qualified as Exts
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

import Data.ByteString.Short (ShortByteString (..))
import Prelude hiding (putStr, putStrLn)

{- | Yields a pinned ByteArray whose contents are identical to those
of the original byte sequence. If the @ByteArray@ backing the argument
was already pinned, this simply aliases the argument and does not perform
any copying.
-}
pin :: ByteArray -> ByteArray
pin b =
  if PM.isByteArrayPinned b
    then b
    else
      let len = PM.sizeofByteArray b
       in runST $ do
            dst <- PM.newPinnedByteArray len
            PM.copyByteArray dst 0 b 0 len
            PM.unsafeFreezeByteArray dst

-- Trusting original byteslice impl
touchByteArrayIO :: ByteArray -> IO ()
touchByteArrayIO (ByteArray x) =
  IO (\s -> (# Exts.touch# x s, () #))

{- | Outputs @OsString@ to the specified @Handle@. This is implemented
with 'System.IO.hPutBuf'.
-}
hPut :: Handle -> OsString -> IO ()
hPut h os = do
  let b1 = pin (coerce os)
  IO.hPutBuf h (PM.byteArrayContents b1) (PM.sizeofByteArray b1)
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
