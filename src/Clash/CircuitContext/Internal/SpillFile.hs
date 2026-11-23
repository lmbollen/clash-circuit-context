{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The regions of a spill file (the format is in
"Clash.CircuitContext.Internal.Recorder"). A region's disk space is reserved
first, then the region is mapped into memory, shared with the file, so that
a store into it is in the file at once (in the kernel's page cache), also
when the process is killed right after. Mapped memory is outside the GHC
heap.

Where a region cannot be mapped (a file system without shared writable
mappings, more mappings than the system allows, Windows), it is a buffer in
memory instead, written to the file by 'persistBlock' and 'retire'.

The system calls are in @cbits/ccc_spill.c@. Each returns 0 or an error
number.
-}
module Clash.CircuitContext.Internal.SpillFile (
  Fd,
  Region (..),
  headerBytes,
  pageSize,
  errorMessage,
  openSpill,
  closeSpill,
  reserve,
  writeAt,
  newRegion,
  setUsed,
  setProgress,
  persistBlock,
  retire,
) where

import Data.Int (Int64)
import Data.Word (Word64, Word8)
import Foreign.C.Error (Errno (Errno), errnoToIOError)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca, callocBytes, free)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (peek, pokeByteOff)
import System.IO.Unsafe (unsafePerformIO)

import qualified Control.Exception as Exception

foreign import ccall safe "ccc_spill_open" c_open :: CString -> IO CInt
foreign import ccall safe "ccc_spill_reserve"
  c_reserve :: CInt -> Int64 -> Int64 -> IO CInt
foreign import ccall safe "ccc_spill_map"
  c_map :: CInt -> Int64 -> Int64 -> Ptr (Ptr Word8) -> IO CInt
foreign import ccall unsafe "ccc_spill_unmap" c_unmap :: Ptr Word8 -> Int64 -> IO CInt
foreign import ccall safe "ccc_spill_write"
  c_write :: CInt -> Ptr Word8 -> Int64 -> Int64 -> IO CInt
foreign import ccall safe "ccc_spill_close" c_close :: CInt -> IO CInt
foreign import ccall unsafe "ccc_spill_page_size" c_pageSize :: IO Int64

-- | An open spill file.
type Fd = CInt

{- | A region of the spill file: its offset and size, both multiples of the
'pageSize', and the memory its bytes are in.
-}
data Region = Region
  { regionOffset :: !Int
  , regionSize :: !Int
  , regionPtr :: !(Ptr Word8)
  , regionMapped :: !Bool
  -- ^ 'False': a buffer, written to the file by 'persistBlock' and 'retire'.
  }

{- | The bytes of a region's header: four 64-bit little-endian numbers, the
region's size, its owner, the number of bytes used after the header, and
its progress.
-}
headerBytes :: Int
headerBytes = 32

-- | The size a region's offset and size are multiples of.
pageSize :: Int
pageSize = fromIntegral (unsafePerformIO c_pageSize)
{-# NOINLINE pageSize #-}

-- | What an error number of this call on the file at this path means.
errorMessage :: FilePath -> String -> CInt -> String
errorMessage path call e =
  Exception.displayException (errnoToIOError call (Errno e) Nothing (Just path))

-- | Open a spill file: made, or truncated. 'Left' with the error number.
openSpill :: FilePath -> IO (Either CInt Fd)
openSpill path = do
  fd <- withCString path c_open
  pure (if fd < 0 then Left (negate fd) else Right fd)

closeSpill :: Fd -> IO ()
closeSpill fd = () <$ c_close fd

result :: CInt -> Either CInt ()
result 0 = Right ()
result e = Left e

-- | Reserve the disk space of this many bytes at this offset.
reserve :: Fd -> Int -> Int -> IO (Either CInt ())
reserve fd off len = result <$> c_reserve fd (fromIntegral off) (fromIntegral len)

-- | Write this many bytes at this offset.
writeAt :: Fd -> Ptr Word8 -> Int -> Int -> IO (Either CInt ())
writeAt fd p len off = result <$> c_write fd p (fromIntegral len) (fromIntegral off)

{- | A new region of the file, at this offset, of this size, for this owner.
Its header has its size and owner, in the file at once.

Mapped: its disk space is reserved first, so that no store into it can find
the disk full. Where it cannot be mapped (or mapping is not asked for), it
is a buffer instead, with the error of the mapping (0: not asked for). A
buffer's disk space is not reserved: writing it says whether the disk is
full.

'Left', with the call that failed and its error, when the disk space cannot
be reserved, or a buffer's header cannot be written. The file then has no
region there, but can have grown.
-}
newRegion ::
  Fd -> Bool -> Int -> Int -> Int -> IO (Either (String, CInt) (Region, Maybe CInt))
newRegion fd mapping off size owner
  | mapping = do
      reserved <- reserve fd off size
      case reserved of
        Left e -> pure (Left ("posix_fallocate", e))
        Right () -> do
          mapped <- alloca $ \out -> do
            e <- c_map fd (fromIntegral off) (fromIntegral size) out
            p <- peek out
            pure (if e == 0 && p /= nullPtr then Right p else Left e)
          case mapped of
            Right p -> do
              header p
              pure (Right (Region off size p True, Nothing))
            Left e -> buffer e
  | otherwise = buffer 0
 where
  buffer e = do
    p <- callocBytes size
    header p
    written <- writeAt fd p headerBytes off
    case written of
      Right () -> pure (Right (Region off size p False, Just e))
      Left w -> free p >> pure (Left ("write", w))
  header p = do
    pokeByteOff p 0 (fromIntegral size :: Word64)
    pokeByteOff p 8 (fromIntegral owner :: Word64)

-- | Store the number of bytes used after the header: after storing them.
setUsed :: Region -> Int -> IO ()
setUsed r n = pokeByteOff (regionPtr r) 16 (fromIntegral n :: Word64)
{-# INLINE setUsed #-}

-- | Store the cycle up to which the region's signal has recorded.
setProgress :: Region -> Int -> IO ()
setProgress r n = pokeByteOff (regionPtr r) 24 (fromIntegral n :: Word64)
{-# INLINE setProgress #-}

{- | Put the bytes @[from, to)@ after a region's header, and the header, in
the file: nothing to do for a mapped region.
-}
persistBlock :: Fd -> Region -> Int -> Int -> IO (Either CInt ())
persistBlock fd r from to
  | regionMapped r = pure (Right ())
  | otherwise = do
      body <-
        writeAt
          fd
          (regionPtr r `plusPtr` (headerBytes + from))
          (to - from)
          (regionOffset r + headerBytes + from)
      case body of
        Left e -> pure (Left e)
        Right () -> writeAt fd (regionPtr r) headerBytes (regionOffset r)

{- | Done with a region whose header and first bytes (this many after the
header) are final: unmap it, or write and free its buffer.
-}
retire :: Fd -> Region -> Int -> IO (Either CInt ())
retire fd r used
  | regionMapped r = result <$> c_unmap (regionPtr r) (fromIntegral (regionSize r))
  | otherwise = do
      written <- writeAt fd (regionPtr r) (headerBytes + used) (regionOffset r)
      free (regionPtr r)
      pure written
