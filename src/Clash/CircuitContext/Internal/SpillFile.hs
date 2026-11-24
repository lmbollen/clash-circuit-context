{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The regions of a spill file (the format is in
"Clash.CircuitContext.Internal.Recorder"). On Linux, a region's disk space
is reserved first, then the region is mapped into memory, shared with the
file, so that a store into it is in the file at once (in the kernel's page
cache), also when the process is killed right after. Mapped memory is
outside the GHC heap. The system calls are in @cbits/ccc_spill.c@.

Where a region cannot be mapped (a file system without shared writable
mappings, more mappings than the system allows), and on every platform but
Linux, it is a buffer in memory instead, written to the file by
'persistBlock' and 'retire'. Elsewhere than on Linux, the file is a
'System.IO.Handle', and nothing is mapped or reserved.

A recording locks its spill file while it has it open, so that a second
recording with the same path is refused ('inUse') instead of truncating
the file the first one writes: a region that the first one has mapped is
then past the end of the file, and its next store into it raises @SIGBUS@,
which kills the process. On Linux, the lock is an exclusive @flock@, which
two opens in one process also exclude each other with. Elsewhere, it is
'GHC.IO.Handle.Lock.hTryLock' where the platform can lock a file, and else
only GHC's own lock of a file that a 'System.IO.Handle' writes, which holds
in one process.
-}
module Clash.CircuitContext.Internal.SpillFile (
  Fd,
  Error,
  Region (..),
  mapping,
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

import Data.IORef (IORef, newIORef)
import Data.Word (Word64, Word8)
import Foreign.Marshal.Alloc (callocBytes, free)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.IO.Exception (IOErrorType (ResourceBusy))
import System.IO.Error (ioeSetErrorString, mkIOError)
import System.IO.Unsafe (unsafePerformIO)

import qualified Control.Exception as Exception

#if defined(linux_HOST_OS)
import Data.Int (Int64)
import Foreign.C.Error (Errno (Errno), errnoToIOError)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)

foreign import ccall safe "ccc_spill_open" c_open :: CString -> Ptr CInt -> IO CInt
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

-- | Why a call failed: its error number.
type Error = CInt

-- | Whether this platform maps a spill file: Linux does.
mappable :: Bool
mappable = True

-- | The size a region's offset and size are multiples of.
pageSize :: Int
pageSize = fromIntegral (unsafePerformIO c_pageSize)
{-# NOINLINE pageSize #-}

-- | What an error of this call on the file at this path means.
errorMessage :: FilePath -> String -> Error -> String
errorMessage path call e =
  Exception.displayException (errnoToIOError call (Errno e) Nothing (Just path))

{- | Open a spill file, and lock it: made, or truncated. Throws when it
cannot be, and 'inUse' when another recording has it open.
-}
openSpill :: FilePath -> IO Fd
openSpill path = withCString path $ \cpath -> alloca $ \busy -> do
  fd <- c_open cpath busy
  locked <- peek busy
  if
    | locked /= 0 -> ioError (inUse path)
    | fd < 0 -> ioError (errnoToIOError "open" (Errno (negate fd)) Nothing (Just path))
    | otherwise -> pure fd

closeSpill :: Fd -> IO ()
closeSpill fd = () <$ c_close fd

result :: CInt -> Either Error ()
result 0 = Right ()
result e = Left e

-- | Reserve the disk space of this many bytes at this offset.
reserve :: Fd -> Int -> Int -> IO (Either Error ())
reserve fd off len = result <$> c_reserve fd (fromIntegral off) (fromIntegral len)

-- | Write this many bytes at this offset.
writeAt :: Fd -> Ptr Word8 -> Int -> Int -> IO (Either Error ())
writeAt fd p len off = result <$> c_write fd p (fromIntegral len) (fromIntegral off)

-- | Map this many bytes at this offset into memory, shared with the file.
mapAt :: Fd -> Int -> Int -> IO (Either Error (Ptr Word8))
mapAt fd off len = alloca $ \out -> do
  e <- c_map fd (fromIntegral off) (fromIntegral len) out
  p <- peek out
  pure (if e == 0 && p /= nullPtr then Right p else Left e)

-- | Unmap this many bytes mapped at this address.
unmap :: Ptr Word8 -> Int -> IO (Either Error ())
unmap p len = result <$> c_unmap p (fromIntegral len)
#else
import Control.Exception (IOException, try)
import GHC.IO.Handle.Lock (
  FileLockingNotSupported (FileLockingNotSupported),
  LockMode (ExclusiveLock),
  hTryLock,
 )
import System.IO.Error (isAlreadyInUseError)
import System.IO (
  BufferMode (NoBuffering),
  Handle,
  IOMode (ReadWriteMode),
  SeekMode (AbsoluteSeek),
  hClose,
  hPutBuf,
  hSeek,
  hSetBuffering,
  hSetFileSize,
  openBinaryFile,
 )

-- | An open spill file.
type Fd = Handle

-- | Why a call failed.
type Error = IOException

-- | Whether this platform maps a spill file: only Linux does.
mappable :: Bool
mappable = False

-- | The size a region's offset and size are multiples of.
pageSize :: Int
pageSize = 4096

-- | What an error means: it names the file and the call itself.
errorMessage :: FilePath -> String -> Error -> String
errorMessage _ _ = Exception.displayException

{- | Open a spill file, and lock it: made, or truncated. Throws when it
cannot be, and 'inUse' when another recording has it open: in this process
(GHC locks a file that a 'Handle' writes), or, where the platform can lock
a file, in another one.
-}
openSpill :: FilePath -> IO Fd
openSpill path = do
  h <-
    openBinaryFile path ReadWriteMode `Exception.catch` \e ->
      ioError (if isAlreadyInUseError e then inUse path else e)
  ( do
      locked <-
        hTryLock h ExclusiveLock `Exception.catch` \FileLockingNotSupported -> pure True
      if locked
        then hSetFileSize h 0 >> hSetBuffering h NoBuffering >> pure h
        else ioError (inUse path)
    )
    `Exception.onException` hClose h

{- | Close the spill file. A write that failed leaves its bytes in the
'Handle', which closing tries again; that error has been reported already.
-}
closeSpill :: Fd -> IO ()
closeSpill h = hClose h `Exception.catch` \(_ :: IOException) -> pure ()

-- | Nothing is reserved here: nothing is mapped.
reserve :: Fd -> Int -> Int -> IO (Either Error ())
reserve _ _ _ = pure (Right ())

-- | Write this many bytes at this offset.
writeAt :: Fd -> Ptr Word8 -> Int -> Int -> IO (Either Error ())
writeAt h p len off = try (hSeek h AbsoluteSeek (fromIntegral off) >> hPutBuf h p len)

-- | Nothing is mapped here.
mapAt :: Fd -> Int -> Int -> IO (Either Error (Ptr Word8))
mapAt _ _ _ = Left <$> unavailable

-- | Nothing is mapped here.
unmap :: Ptr Word8 -> Int -> IO (Either Error ())
unmap _ _ = Left <$> unavailable

unavailable :: IO Error
unavailable = pure (userError "a spill file is mapped on Linux only")
#endif

{- | The error of a spill file that another recording, in this process or
another, has open: two recordings at one path at the same time.
-}
inUse :: FilePath -> IOError
inUse path =
  mkIOError
    ResourceBusy
    ""
    Nothing
    (Just path)
    `ioeSetErrorString` ( "two recordings use one spill file: a recording that is running,"
                            <> " in this process or another, writes it. Give each recording"
                            <> " (each test) a path of its own"
                        )

{- | Whether a new recorder maps its regions: on Linux, unless this
package's tests say otherwise ("Clash.CircuitContext.Internal.SpillTesting").
-}
mapping :: IORef Bool
mapping = unsafePerformIO (newIORef mappable)
{-# NOINLINE mapping #-}

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

{- | A new region of the file, at this offset, of this size, for this owner.
Its header has its size and owner, in the file at once.

Mapped: its disk space is reserved first, so that no store into it can find
the disk full. Where it cannot be mapped (or mapping is not asked for), it
is a buffer instead; then 'Just' the error of the mapping, if it was asked
for. A buffer's disk space is not reserved: writing it says whether the
disk is full.

'Left', with the call that failed and its error, when the disk space cannot
be reserved, or a buffer's header cannot be written. The file then has no
region there, but can have grown.
-}
newRegion ::
  Fd -> Bool -> Int -> Int -> Int -> IO (Either (String, Error) (Region, Maybe Error))
newRegion fd mapIt off size owner
  | mapIt = do
      reserved <- reserve fd off size
      case reserved of
        Left e -> pure (Left ("posix_fallocate", e))
        Right () -> do
          mapped <- mapAt fd off size
          case mapped of
            Right p -> do
              header p
              pure (Right (Region off size p True, Nothing))
            Left e -> buffer (Just e)
  | otherwise = buffer Nothing
 where
  buffer why = do
    p <- callocBytes size
    header p
    written <- writeAt fd p headerBytes off
    case written of
      Right () -> pure (Right (Region off size p False, why))
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
persistBlock :: Fd -> Region -> Int -> Int -> IO (Either Error ())
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
retire :: Fd -> Region -> Int -> IO (Either Error ())
retire fd r used
  | regionMapped r = unmap (regionPtr r) (regionSize r)
  | otherwise = do
      written <- writeAt fd (regionPtr r) (headerBytes + used) (regionOffset r)
      free (regionPtr r)
      pure written
