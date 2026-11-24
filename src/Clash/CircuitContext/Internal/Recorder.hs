{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The recorder's storage: every recorded signal keeps its value CHANGES as
compact records while the simulation runs, either in memory or in a spill
file, so memory does not grow with the length of a run (with a spill file)
or grows by the compact records only (in memory).

A change record is the cycle (as the distance from the previous record, a
varint), a tag, and the value's bytes:

* tag 0: a defined value, @ceil (width / 8)@ bytes, little endian;
* tag 1: a value with undefined bits: the value's bytes, then the mask's;
* tag 2: a gap: from this cycle on, the signal was not evaluated (@z@).

Before its first record, a signal was not evaluated either.

In memory, each signal encodes its changes into a small buffer of its own.
The buffer starts small and doubles as needed, so a signal that rarely
changes costs a few bytes; when it reaches 'chunkBytes', it becomes one
chunk.

== The spill file

With a spill file, each signal encodes its changes into a REGION of the
file, on Linux mapped into memory and shared with the file: a record is in the file
(in the kernel's page cache) as soon as it is stored, so a process that is
killed (SIGKILL, the out-of-memory killer, a crash) leaves everything it
recorded. Only a crash of the machine itself can lose the newest records.
A region's disk space is reserved before it is mapped, so that a full disk
is an error at a known point rather than a SIGBUS at a store: the recording
then stops, says so on stderr, and keeps what it has; the run goes on, and
its waveform ends where the recording stopped.

A signal's first region is a page; each next one is twice the size of the
one before, up to 'regionBytes', so that a signal that rarely changes takes
a page of disk. A full region is unmapped: a signal has one mapping at a
time, and its index in memory grows by one entry per full region.

A spill file describes itself, so that the file of a run that was stopped
can still be read ('readSpill'). It is a header, then regions:

* header, in a page of its own: the 8 bytes @CCCSPILL@, a version byte (2),
  7 zero bytes, the time (ps) from which cycles are recorded, and the offset
  of the first region;
* region: a header of four words, then bytes. The words are the region's
  size (header included; 0: no region from here on), its owner (0: the
  metadata; @i + 1@: signal @i@), the number of bytes used after the header,
  and (a signal's region) the cycle up to which the signal has recorded,
  exclusive. A signal's bytes are its change records; the metadata's are
  blocks.

Every number in a header is 64 bits, little endian. Records and blocks are
stored before the number of bytes used counts them, and the progress after
that, so a killed process leaves whole records and blocks only. Between the
two stores, a signal's last record can be past its progress: a signal has
recorded up to its progress or one past its last record, whichever is later.

A block is a tag byte, the payload's length (32 bits, little endian), and
the payload. Numbers in a payload are varints, strings are a varint length
and UTF-8 bytes.

* 1, a signal: its id, kind (0 trace, 1 probe), clock period (ps), width,
  key, and description: 0 none; 1 described, with the type name and the
  types as JSON; 2 broken, with the error;
* 3, lookup-table entries, as JSON (a @clash-shockwaves@ @LUTMap@);
* 4, the end: the time (ps) at which the simulation stopped;
* 5, a signal's description broke after it registered: its id, and the
  error.

A signal's block comes before its records, in a region before its first.

Where a region cannot be mapped (a file system without shared writable
mappings, more mappings than the system allows), and on every platform but
Linux, it is a buffer
in memory instead, written to the file when it is full. A killed process
then loses each signal's last region: up to 16 KiB of its newest records,
and, for a signal that changes rarely and never fills its first region,
everything it recorded. Such a signal shows as not evaluated in the
recovered waveform. A metadata block is written at once either way. A buffer's disk space is not reserved: when the
disk is full, writing it fails, its records are lost, and the recording
stops; the signal then ends where the lost region began.

A finished 'Recording' reads its changes back lazily, one chunk (in a spill
file: one region) at a time ('readChanges'), so rendering does not hold a
history in memory either.

== Descriptions

A signal's @clash-shockwaves@ description is rendered when the signal
registers, and the lookup-table entries a value needs when the value first
occurs, so that a spill file carries everything the typed-waveform sidecar
needs (see "Clash.CircuitContext.Internal.Sidecar"). A description that
throws is kept as 'Broken', with its error: the signal is still recorded, as
bits.
-}
module Clash.CircuitContext.Internal.Recorder (
  -- * Values
  Value,

  -- * Recording
  Kind (..),
  Description (..),
  Recorder,
  newRecorder,
  recorderSpill,
  Sig,
  sigPeriod,
  newSig,
  recordCycle,
  recorderEnd,
  closeRecorder,

  -- * A finished recording
  Recording (..),
  holdsCycles,
  RecordedSignal (..),
  Chunk,
  Change (..),
  Reader,
  withReader,
  readChanges,
  readSpill,
  timescale,
  ticks,
) where

import Control.Concurrent (myThreadId, throwTo)
import Control.Concurrent.MVar (
  MVar,
  modifyMVar,
  modifyMVar_,
  newMVar,
  putMVar,
  readMVar,
  takeMVar,
 )
import Control.Exception (
  SomeAsyncException,
  SomeException,
  bracket,
  displayException,
  evaluate,
  finally,
  fromException,
  onException,
  try,
  uninterruptibleMask_,
 )
import Control.Monad (forM, unless, when)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Word (Word64, Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Natural (Natural)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (
  Handle,
  IOMode (ReadMode),
  SeekMode (AbsoluteSeek),
  hClose,
  hFileSize,
  hPutStrLn,
  hSeek,
  openBinaryFile,
  stderr,
 )
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Control.Exception as Exception
import qualified Data.Aeson as Json
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.ByteString.Internal as ByteString (create)
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Clash.CircuitContext.Internal.SpillFile (
  Fd,
  Region (..),
  closeSpill,
  errorMessage,
  headerBytes,
  mapping,
  newRegion,
  openSpill,
  pageSize,
  persistBlock,
  reserve,
  retire,
  setProgress,
  setUsed,
  writeAt,
 )
import Clash.Shockwaves.Internal.BitList (BitList (BL))
import Clash.Shockwaves.Internal.Translator (addTypesT, addValueT, hasGeneratedLutT)
import Clash.Shockwaves.Internal.Types (Translator, TypeName)

{- | A sample as the VCD writer needs it: @(mask, value)@, where a set mask
bit is an undefined bit.
-}
type Value = (Natural, Natural)

-- | A traced signal, or a probe inside a mealy machine's step function.
data Kind = TraceKind | ProbeKind
  deriving (Eq, Show)

-- | A signal's @clash-shockwaves@ description, rendered when it registered.
data Description
  = -- | Its payload type has none.
    Undescribed
  | -- | The payload's type name, and the types it uses as JSON (a @TypeMap@).
    Described TypeName ByteString.ByteString
  | -- | Rendering it threw this error.
    Broken String
  deriving (Eq, Show)

-- | Where a recorder's records go.
data Store
  = ToFile !Spill
  | ToMemory
  | -- | The recording ended.
    Closed

-- | An open spill file.
data Spill = Spill
  { spFd :: !Fd
  , spEnd :: !Int
  -- ^ Where the next region goes: the file's end.
  , spMapping :: !Bool
  -- ^ Map regions ('False': buffers; see "Clash.CircuitContext.Internal.SpillTesting").
  , spMeta :: !(Maybe Region)
  -- ^ The region the next metadata block goes to.
  , spMetaUsed :: !Int
  , spMetaNext :: !Int
  -- ^ The size of the next metadata region.
  , spFellBack :: !Bool
  -- ^ A region could not be mapped, and the recorder said so.
  }

-- | Why a recorder records nothing more.
data Halt
  = Closing
  | -- | The spill file cannot grow.
    Full
  deriving (Eq)

-- | Where a running simulation records. Its signals share one store.
data Recorder = Recorder
  { rSpill :: Maybe FilePath
  , rStart :: !Int
  -- ^ The time, in ps, from which on cycles are recorded.
  , rStore :: MVar Store
  , rSignals :: IORef [Sig]
  -- ^ Every signal registered, newest first.
  , rLuts :: IORef [ByteString.ByteString]
  -- ^ The lookup-table entries, as JSON, newest first.
  , rNextId :: IORef Int
  , rHalt :: IORef (Maybe Halt)
  }

-- | One recorded signal, while the simulation runs.
data Sig = Sig
  { sigId :: !Int
  , sigKey :: !String
  , sigKind :: Kind
  , sigPeriod :: !Int
  , sigWidth :: !Int
  , sigDescription :: Description
  , sigLut :: Maybe Translator
  -- ^ When its description has lookup tables: the translator that fills them.
  , sigStart :: !Int
  -- ^ The first cycle that is recorded: the one in progress at 'rStart'.
  , sigState :: MVar SigState
  }

data SigState = SigState
  { ssNext :: !Int
  -- ^ Cycles @[0, next)@ are recorded (or were skipped, as a gap).
  , ssLast :: !(Maybe Value)
  -- ^ The value of the newest recorded cycle; 'Nothing' in a gap.
  , ssFresh :: !Bool
  -- ^ Nothing is written yet.
  , ssLastChange :: !Int
  -- ^ The cycle of the newest record: the base of the next record's delta.
  , ssBuf :: !Buf
  , ssLen :: !Int
  -- ^ The bytes of records in the buffer.
  , ssChunks :: ![Chunk]
  -- ^ The finished chunks, newest first.
  , ssHash :: !Word64
  -- ^ A hash of every record, to find signals with the same history.
  , ssSeen :: !(Set.Set Value)
  -- ^ The values whose lookup-table entries are made ('sigLut' only).
  , ssBroken :: !(Maybe String)
  -- ^ Making a lookup-table entry threw this error.
  , ssLost :: !(Maybe Int)
  {- ^ A region could not be written (a buffer, on a full disk): the
  records from this cycle on are lost.
  -}
  }

-- | Where a signal's newest records are.
data Buf
  = -- | In memory: a buffer of this capacity.
    InHeap !(ForeignPtr Word8) !Int
  | {- | In the spill file: in this region ('Nothing' before the first
    record, and after the last), and the size of the next region.
    -}
    InRegion !(Maybe Live) !Int

-- | A signal's region, and the cycle from which on its records are in it.
data Live = Live !Region !Int

-- | A chunk of a signal's records.
data Chunk
  = -- | At this offset of the spill file, this many bytes.
    OnFile !Int !Int
  | InMemory !ByteString.ByteString

-- | The size at which a signal's buffer in memory becomes a chunk.
chunkBytes :: Int
chunkBytes = 16 * 1024

-- | The size of a full-grown region of the spill file.
regionBytes :: Int
regionBytes = 16 * 1024

-- | The first bytes of a spill file.
magic :: ByteString.ByteString
magic = ByteStringChar8.pack "CCCSPILL"

spillVersion :: Word8
spillVersion = 2

{- | A recorder that records the cycles in progress at or after this time (in
ps): into a spill file at this path (made, with its directory; an existing
file is truncated), or, given 'Nothing', in memory.
-}
newRecorder :: Maybe FilePath -> Int -> IO Recorder
newRecorder spill start = do
  store <- case spill of
    Nothing -> pure ToMemory
    Just path -> do
      createDirectoryIfMissing True (takeDirectory path)
      fd <- openSpill path
      mapped <- readIORef mapping
      pure (ToFile (Spill fd pageSize mapped Nothing 0 pageSize False))
  rec <-
    Recorder spill start
      <$> newMVar store
      <*> newIORef []
      <*> newIORef []
      <*> newIORef 0
      <*> newIORef Nothing
  case store of
    ToFile sp -> do
      written <- writeHeader (spFd sp) `onException` closeSpill (spFd sp)
      either (halt rec Full) pure written
    _ -> pure ()
  pure rec
 where
  -- The header, in a page of its own, reserved like a region.
  writeHeader fd = do
    reserved <- reserve fd 0 pageSize
    case reserved of
      Left e -> pure (Left (errorMessage (fromMaybe "" spill) "posix_fallocate" e))
      Right () -> do
        let header =
              ByteStringLazy.toStrict . Builder.toLazyByteString $
                Builder.byteString magic
                  <> Builder.word8 spillVersion
                  <> Builder.byteString (ByteString.replicate 7 0)
                  <> Builder.word64LE (fromIntegral start)
                  <> Builder.word64LE (fromIntegral pageSize)
        w <- ByteString.unsafeUseAsCString header $ \p ->
          writeAt fd (castPtr p) (ByteString.length header) 0
        pure (either (Left . errorMessage (fromMaybe "" spill) "write") Right w)

{- | Record nothing more, for this reason. A full spill file counts also
while the recorder closes, and says so on stderr, once.
-}
halt :: Recorder -> Halt -> String -> IO ()
halt rec why err = do
  full <- atomicModifyIORef' (rHalt rec) $ \h -> case (h, why) of
    (Just Full, _) -> (h, False)
    (_, Full) -> (Just Full, True)
    _ -> (Just (fromMaybe why h), False)
  when full $
    hPutStrLn
      stderr
      ( "clash-circuit-context: "
          <> err
          <> ". The spill file cannot grow, so the recording stops here; the run"
          <> " goes on, and its waveform ends where the recording stopped"
      )

-- | The recorder's spill file, if it has one.
recorderSpill :: Recorder -> Maybe FilePath
recorderSpill = rSpill

{- | A new region at the end of the spill file, for this owner, of this size
or more, with room for this many bytes: 'Nothing' when the file cannot grow
(the recording then stops).
-}
allocate :: Recorder -> Spill -> Int -> Int -> Int -> IO (Spill, Maybe Region)
allocate rec sp owner want need = do
  let size = roundUp (max want (headerBytes + need))
  made <- newRegion (spFd sp) (spMapping sp) (spEnd sp) size owner
  case made of
    Left err -> (sp, Nothing) <$ halt rec Full (path err)
    Right (r, fellBack) -> do
      said <- case fellBack of
        Just e
          | spMapping sp
          , not (spFellBack sp) -> do
              hPutStrLn
                stderr
                ( "clash-circuit-context: "
                    <> errorMessage (fromMaybe "" (rSpill rec)) "mmap" e
                    <> ". The spill file is written a region at a time instead, so a"
                    <> " process that is killed loses each signal's newest records"
                )
              pure True
        _ -> pure (spFellBack sp)
      pure (sp{spEnd = spEnd sp + size, spFellBack = said}, Just r)
 where
  roundUp n = (n + pageSize - 1) `div` pageSize * pageSize
  path (loc, e) = errorMessage (fromMaybe "" (rSpill rec)) loc e

{- | Done with a region that holds this many bytes: unmapped, or written.
'False' when it cannot be written (the recording then stops).
-}
retireRegion :: Recorder -> Fd -> Region -> Int -> IO Bool
retireRegion rec fd r used = do
  done <- retire fd r used
  case done of
    Right () -> pure True
    Left e -> False <$ halt rec Full (errorMessage (fromMaybe "" (rSpill rec)) call e)
 where
  call = if regionMapped r then "munmap" else "write"

{- | Append a block to the spill file's metadata: this tag, and this
payload. Nothing without a spill file, or once the recording ended.

A block is written whole, also when the run is cancelled while it is
written.
-}
writeBlock :: Recorder -> Word8 -> Builder.Builder -> IO ()
writeBlock rec tag payload =
  uninterruptibleMask_ $ modifyMVar_ (rStore rec) $ \store -> case store of
    ToFile sp -> ToFile <$> append sp
    _ -> pure store
 where
  body = ByteStringLazy.toStrict (Builder.toLazyByteString payload)
  bytes' =
    ByteStringLazy.toStrict . Builder.toLazyByteString $
      Builder.word8 tag
        <> Builder.word32LE (fromIntegral (ByteString.length body))
        <> Builder.byteString body
  len = ByteString.length bytes'
  append sp0 = do
    (sp, target) <- case spMeta sp0 of
      Just r | spMetaUsed sp0 + len <= regionSize r - headerBytes -> pure (sp0, Just r)
      old -> do
        -- The file stops growing where it could not.
        halted <- readIORef (rHalt rec)
        (sp1, new) <-
          if halted == Just Full
            then pure (sp0, Nothing)
            else allocate rec sp0 0 (spMetaNext sp0) len
        case new of
          Nothing -> pure (sp1, Nothing)
          Just r -> do
            mapM_ (\o -> retireRegion rec (spFd sp1) o (spMetaUsed sp1)) old
            pure
              ( sp1
                  { spMeta = Just r
                  , spMetaUsed = 0
                  , spMetaNext = min regionBytes (2 * regionSize r)
                  }
              , Just r
              )
    case target of
      Nothing -> pure sp
      Just r -> do
        let at = spMetaUsed sp
        ByteString.unsafeUseAsCString bytes' $ \p ->
          copyBytes (regionPtr r `plusPtr` (headerBytes + at)) (castPtr p) len
        setUsed r (at + len)
        persisted <- persistBlock (spFd sp) r at (at + len)
        either (halt rec Full . errorMessage (fromMaybe "" (rSpill rec)) "write") pure persisted
        pure sp{spMetaUsed = at + len}

varint :: Int -> Builder.Builder
varint x
  | x < 0x80 = Builder.word8 (fromIntegral x)
  | otherwise = Builder.word8 (fromIntegral (x .&. 0x7f) .|. 0x80) <> varint (x `shiftR` 7)

string :: String -> Builder.Builder
string = bytes . Text.encodeUtf8 . Text.pack

bytes :: ByteString.ByteString -> Builder.Builder
bytes b = varint (ByteString.length b) <> Builder.byteString b

{- | Register a signal: its key, kind, clock period (ps), width and
@clash-shockwaves@ description (rendered here; see 'Description').
-}
newSig ::
  Recorder -> String -> Kind -> Int -> Int -> Maybe (TypeName, Translator) -> IO Sig
newSig rec key kind period width adt = do
  -- A key is built lazily from the design (a generic field name holds the
  -- field's value), so an unevaluated one keeps the signal it names alive,
  -- with every cycle the simulation gives it.
  _ <- evaluate (foldr seq () key)
  (desc, lut) <- describe adt
  i <- atomicModifyIORef' (rNextId rec) (\n -> (n + 1, n))
  store <- readMVar (rStore rec)
  buf <- case store of
    ToMemory -> (`InHeap` cap) <$> mallocForeignPtrBytes cap
    _ -> pure (InRegion Nothing pageSize)
  st <-
    newMVar
      (SigState 0 Nothing True 0 buf 0 [] 14695981039346656037 Set.empty Nothing Nothing)
  let sig = Sig i key kind period width desc lut (rStart rec `div` period) st
  writeBlock
    rec
    1
    ( varint i
        <> Builder.word8 (if kind == TraceKind then 0 else 1)
        <> varint period
        <> varint width
        <> string key
        <> case desc of
          Undescribed -> Builder.word8 0
          Described tyName types -> Builder.word8 1 <> string tyName <> bytes types
          Broken err -> Builder.word8 2 <> string err
    )
  atomicModifyIORef' (rSignals rec) (\ss -> (sig : ss, ()))
  pure sig
 where
  -- Room for the two records one cycle can write (a gap, then a value).
  cap = 64 + 2 * recordBytes width

{- | Render a description, and whether it has lookup tables (then also its
translator). One that throws is 'Broken'; a cancellation goes on.
-}
describe :: Maybe (TypeName, Translator) -> IO (Description, Maybe Translator)
describe Nothing = pure (Undescribed, Nothing)
describe (Just (tyName, tr)) = do
  rendered <- tryRender $ do
    let types = ByteStringLazy.toStrict (Json.encode (addTypesT tr Map.empty))
        lut = hasGeneratedLutT tr
    _ <- evaluate (length tyName)
    _ <- evaluate (ByteString.length types)
    (,) types <$> evaluate lut
  pure $ case rendered of
    Right (types, lut) -> (Described tyName types, if lut then Just tr else Nothing)
    Left err -> (Broken err, Nothing)

{- | Run a rendering, catching what it throws, but not a cancellation. A
rendering runs inside the simulation's thunks, so a cancellation goes on as
one, thrown to this thread (see 'Clash.CircuitContext.Core.packMaskValue');
resumed, the rendering starts over.
-}
tryRender :: IO a -> IO (Either String a)
tryRender act = do
  r <- try act
  case r of
    Right a -> pure (Right a)
    Left err | Just (_ :: SomeAsyncException) <- fromException err -> do
      me <- myThreadId
      throwTo me err
      tryRender act
    Left (err :: SomeException) -> pure (Left (takeWhile (/= '\n') (displayException err)))

-- | The most bytes one record of this width takes.
recordBytes :: Int -> Int
recordBytes width = 11 + 2 * ((width + 7) `div` 8)

{- | The time (ps) up to which the signals have recorded so far: one past
the newest cycle of the furthest one.
-}
recorderEnd :: Recorder -> IO Int
recorderEnd rec = do
  sigs <- readIORef (rSignals rec)
  ends <- mapM (\sig -> (* sigPeriod sig) . ssNext <$> readMVar (sigState sig)) sigs
  pure (maximum (0 : ends))

{- | Record the value of one cycle. Cycles must arrive in increasing order: a
cycle that is already recorded is ignored (a lazy re-evaluation), and a cycle
after a skipped one starts with a gap. An unchanged value costs a comparison.
-}
recordCycle :: Recorder -> Sig -> Int -> Value -> IO ()
recordCycle rec sig cyc v = Exception.mask $ \restore -> do
  -- A cycle is recorded whole or not at all: a cancellation that came between
  -- writing a chunk and keeping the state that says so would write it twice.
  -- Only the lookup-table entries, which run the payload type's own code,
  -- can be interrupted; the state is then as it was after 'room'.
  s0 <- takeMVar (sigState sig)
  halted <- readIORef (rHalt rec)
  (s, roomy) <-
    if isJust halted
      then pure (s0, False)
      else
        if cyc < max (ssNext s0) (sigStart sig) || hasRoom s0
          then pure (s0, True)
          else uninterruptibleMask_ (room rec sig s0) `onException` putMVar (sigState sig) s0
  if not roomy
    then -- The recording ended, or the spill file cannot grow: drop the cycle.
      putMVar (sigState sig) s
    else do
      stepped <- try (step restore s)
      case stepped of
        Right s' -> commit s' >> putMVar (sigState sig) s'
        Left err -> do
          putMVar (sigState sig) s
          case fromException err of
            -- A cancellation goes on as one, thrown to this thread, as in
            -- 'tryRender': this runs inside the design's values, and thrown with
            -- throwIO it would be stored in them. Resumed, the cycle starts over.
            Just (_ :: SomeAsyncException) -> do
              me <- myThreadId
              throwTo me err
              recordCycle rec sig cyc v
            Nothing -> Exception.throwIO err
 where
  -- Checked here, without masking and catching, as nearly every cycle has
  -- room: 'room' itself is for when it has not.
  hasRoom s = case ssBuf s of
    InHeap _ cap -> ssLen s + need <= cap
    InRegion (Just (Live r _)) _ -> ssLen s + need <= regionSize r - headerBytes
    InRegion Nothing _ -> False
  need = 2 * recordBytes (sigWidth sig)
  step restore s
    | cyc < ssNext s = pure s
    -- Before the window only the value is kept, for the first record.
    | cyc < sigStart sig = pure s{ssNext = cyc + 1, ssLast = Just v}
    | otherwise = do
        let gapAt = max (ssNext s) (sigStart sig)
        s1 <-
          if cyc > gapAt && isJust (ssLast s)
            then emit sig gapAt Nothing s
            else pure s
        s2 <-
          if not (ssFresh s1) && ssLast s1 == Just v
            then pure s1
            else emit sig cyc (Just v) s1 >>= restore . lutEntries rec sig v
        let s3 = s2{ssNext = cyc + 1}
        case ssBuf s3 of
          InHeap{} | ssLen s3 >= chunkBytes -> chunkInMemory s3
          _ -> pure s3
  -- The records are stored; now the region's header counts them.
  commit s = case ssBuf s of
    InRegion (Just (Live r _)) _ -> setUsed r (ssLen s) >> setProgress r (ssNext s)
    _ -> pure ()

{- | Make room in the signal's buffer for the records of one cycle: grow it
(in memory), or start a new region (in the spill file; the full one is
retired). 'False' when there is no room: the spill file cannot grow, or the
recording ended. The state is the signal's either way.
-}
room :: Recorder -> Sig -> SigState -> IO (SigState, Bool)
room rec sig s = case ssBuf s of
  InHeap buf cap
    | ssLen s + need <= cap -> pure (s, True)
    | otherwise -> do
        let cap' = until (>= ssLen s + need) (* 2) cap
        buf' <- mallocForeignPtrBytes cap'
        withForeignPtr buf $ \p -> withForeignPtr buf' $ \p' -> copyBytes p' p (ssLen s)
        pure (s{ssBuf = InHeap buf' cap'}, True)
  InRegion (Just (Live r _)) _ | ssLen s + need <= regionSize r - headerBytes -> pure (s, True)
  InRegion _ next -> do
    new <- modifyMVar (rStore rec) $ \store -> case store of
      ToFile sp -> do
        (sp', r) <- allocate rec sp (sigId sig + 1) next need
        pure (ToFile sp', (\r' -> (spFd sp', r')) <$> r)
      _ -> pure (store, Nothing)
    case new of
      Nothing -> pure (s, False)
      Just (fd, r) -> do
        s' <- retireLive rec fd s
        let fresh = s'{ssBuf = InRegion (Just (Live r (ssNext s))) (min regionBytes (2 * regionSize r))}
        -- Retiring the full region can fail too (a buffer, on a full disk).
        halted <- readIORef (rHalt rec)
        pure (fresh, not (isJust halted))
 where
  need = 2 * recordBytes (sigWidth sig)

{- | Retire the signal's region, if it has one: its records become a chunk,
or, when the region cannot be written, are lost ('ssLost').
-}
retireLive :: Recorder -> Fd -> SigState -> IO SigState
retireLive rec fd s = case ssBuf s of
  InRegion (Just (Live r from)) next -> do
    written <- retireRegion rec fd r (ssLen s)
    pure
      s
        { ssBuf = InRegion Nothing next
        , ssLen = 0
        , ssChunks =
            if written && ssLen s > 0
              then OnFile (regionOffset r + headerBytes) (ssLen s) : ssChunks s
              else ssChunks s
        , ssLost = if written then ssLost s else Just (maybe from (min from) (ssLost s))
        }
  _ -> pure s

-- | Append one record to the signal's buffer, which has room for it ('room').
emit :: Sig -> Int -> Maybe Value -> SigState -> IO SigState
emit sig c mv s = do
  (o, h) <- withRecords (ssBuf s) $ \p -> do
    let
      put (!o, !h) w = pokeByteOff p o w >> pure (o + 1, fnv h w)
      putVar acc x
        | x < 0x80 = put acc (fromIntegral x)
        | otherwise =
            put acc (fromIntegral (x .&. 0x7f) .|. 0x80) >>= \a -> putVar a (x `shiftR` 7)
      putNat acc 0 _ = pure acc
      putNat acc k x = put acc (fromIntegral (x .&. 0xff)) >>= \a -> putNat a (k - 1 :: Int) (x `shiftR` 8)
    a1 <- putVar (ssLen s, ssHash s) (c - ssLastChange s)
    case mv of
      Nothing -> put a1 2
      Just (0, val) -> put a1 0 >>= \a -> putNat a n val
      Just (mask, val) -> put a1 1 >>= \a -> putNat a n val >>= \a' -> putNat a' n mask
  pure s{ssLen = o, ssHash = h, ssLast = mv, ssLastChange = c, ssFresh = False}
 where
  n = (sigWidth sig + 7) `div` 8

-- | Run an action with the address of a buffer's records.
withRecords :: Buf -> (Ptr Word8 -> IO a) -> IO a
withRecords (InHeap buf _) k = withForeignPtr buf k
withRecords (InRegion (Just (Live r _)) _) k = k (regionPtr r `plusPtr` headerBytes)
withRecords (InRegion Nothing _) _ =
  ioError (userError "clash-circuit-context: a record without a region")

{- | Make the lookup-table entries of a value the signal takes for the first
time, when its description has lookup tables. One that throws breaks the
description ('ssBroken'); the value is recorded all the same.
-}
lutEntries :: Recorder -> Sig -> Value -> SigState -> IO SigState
lutEntries rec sig v@(mask, val) s = case sigLut sig of
  Just tr
    | Set.notMember v (ssSeen s)
    , not (isJust (ssBroken s)) -> do
        made <- tryRender $ do
          let entries =
                List.foldl' (\acc f -> f acc) Map.empty (addValueT tr (BL mask val (sigWidth sig)))
          if Map.null entries
            then pure ByteString.empty
            else do
              let json = ByteStringLazy.toStrict (Json.encode entries)
              _ <- evaluate (ByteString.length json)
              pure json
        case made of
          Right json -> do
            unless (ByteString.null json) $ do
              atomicModifyIORef' (rLuts rec) (\ls -> (json : ls, ()))
              writeBlock rec 3 (Builder.byteString json)
            pure s{ssSeen = Set.insert v (ssSeen s)}
          Left err -> do
            writeBlock rec 5 (varint (sigId sig) <> string err)
            pure s{ssBroken = Just err}
  _ -> pure s

-- | Make the signal's buffer in memory a chunk.
chunkInMemory :: SigState -> IO SigState
chunkInMemory s = case ssBuf s of
  InHeap buf _ | ssLen s > 0 -> do
    chunk <-
      withForeignPtr buf (\p -> ByteString.create (ssLen s) (\q -> copyBytes q p (ssLen s)))
    pure s{ssLen = 0, ssChunks = InMemory chunk : ssChunks s}
  _ -> pure s

-- | Finish the signal's buffer: a chunk in memory, or its region retired.
finish :: Recorder -> SigState -> IO SigState
finish rec s = case ssBuf s of
  InHeap{} -> chunkInMemory s
  InRegion Nothing _ -> pure s
  InRegion (Just _) _ -> do
    store <- readMVar (rStore rec)
    case store of
      ToFile sp -> retireLive rec (spFd sp) s
      _ -> pure s

fnv :: Word64 -> Word8 -> Word64
fnv h b = (h `xor` fromIntegral b) * 1099511628211

--------------------------------------------------------------------------------
-- A finished recording
--------------------------------------------------------------------------------

-- | A recorded signal.
data RecordedSignal = RecordedSignal
  { signalKey :: String
  {- ^ The signal's name, with the instance tags of its path
  (@name\@ordinal\@callsite@ per segment); see "Clash.CircuitContext.Core".
  -}
  , signalKind :: Kind
  , signalPeriod :: Int
  -- ^ Clock period of the signal's domain, in ps.
  , signalWidth :: Int
  , signalDescription :: Description
  , signalEnd :: Int
  -- ^ Cycles from this one on were not evaluated.
  , signalChunks :: [Chunk]
  -- ^ Its records, oldest first.
  , signalHash :: Word64
  }

-- | A finished recording: the signals recorded, and where their records are.
data Recording = Recording
  { recordingSpill :: Maybe FilePath
  -- ^ The spill file; 'Nothing' when the records are in memory.
  , recordingSignals :: [RecordedSignal]
  -- ^ In registration order.
  , recordingStartPs :: Int
  -- ^ Cycles before the one in progress at this time (ps) were not recorded.
  , recordingEndPs :: Int
  -- ^ The time (ps) at which the simulation stopped: nothing was evaluated after it.
  , recordingLuts :: [ByteString.ByteString]
  -- ^ The lookup-table entries, as JSON, oldest first.
  }

{- | Did the run record a cycle: one in progress at or after the time from
which it recorded? A recording that holds none has no waveform.
-}
holdsCycles :: Recording -> Bool
holdsCycles rec = recordingEndPs rec > recordingStartPs rec

{- | Finish a recording that ends at this time (ps): finish every buffer,
mark the end in the spill file, and close it. A cycle recorded after this is
dropped. The spill file is closed also when writing to it fails.

A recording that stopped because the spill file could not grow ends where
its furthest signal stopped.
-}
closeRecorder :: Recorder -> Int -> IO Recording
closeRecorder rec end = flip finally closeStore $ do
  -- First: a cycle that comes after this is dropped.
  atomicModifyIORef' (rHalt rec) (\h -> (Just (fromMaybe Closing h), ()))
  sigs <- reverse <$> readIORef (rSignals rec)
  frozen <- forM sigs $ \sig -> modifyMVar (sigState sig) $ \s0 -> do
    s <- finish rec s0
    pure
      ( s
      , RecordedSignal
          { signalKey = sigKey sig
          , signalKind = sigKind sig
          , signalPeriod = sigPeriod sig
          , signalWidth = sigWidth sig
          , signalDescription = maybe (sigDescription sig) Broken (ssBroken s)
          , signalEnd = maybe (ssNext s) (min (ssNext s)) (ssLost s)
          , signalChunks = reverse (ssChunks s)
          , signalHash = ssHash s
          }
      )
  halted <- readIORef (rHalt rec)
  let end'
        | halted == Just Full =
            min end (maximum (0 : [signalEnd s * signalPeriod s | s <- frozen]))
        | otherwise = end
  writeBlock rec 4 (varint end')
  luts <- reverse <$> readIORef (rLuts rec)
  pure (Recording (rSpill rec) frozen (rStart rec) end' luts)
 where
  closeStore = modifyMVar_ (rStore rec) $ \store -> case store of
    ToFile sp -> do
      mapM_ (\r -> retireRegion rec (spFd sp) r (spMetaUsed sp)) (spMeta sp)
      closeSpill (spFd sp)
      pure Closed
    _ -> pure Closed

-- | A record: from this cycle on, the signal has this value ('Nothing': not evaluated).
data Change = Change !Int !(Maybe Value)
  deriving (Eq, Show)

-- | What 'readChanges' reads a recording's chunks through.
newtype Reader = Reader (Maybe Handle)

-- | Run an action with the recording's records open for 'readChanges'.
withReader :: Recording -> (Reader -> IO a) -> IO a
withReader rec k = case recordingSpill rec of
  Nothing -> k (Reader Nothing)
  Just path -> bracket (openBinaryFile path ReadMode) hClose (k . Reader . Just)

{- | A signal's records, oldest first, read lazily: a chunk is read when the
list reaches it. The reader must stay open while the list is consumed, and
nothing else may move it in between (one thread).
-}
readChanges :: Reader -> RecordedSignal -> IO [Change]
readChanges (Reader mh) sig = go 0 (signalChunks sig)
 where
  n = (signalWidth sig + 7) `div` 8
  go _ [] = pure []
  go base (chunk : rest) = unsafeInterleaveIO $ do
    bs <- case (chunk, mh) of
      (InMemory b, _) -> pure b
      (OnFile off len, Just h) -> do
        hSeek h AbsoluteSeek (fromIntegral off)
        ByteString.hGet h len
      (OnFile _ _, Nothing) -> ioError (userError "clash-circuit-context: a spill file's chunk without its file")
    let (changes, base') = decode base bs
    (changes <>) <$> go base' rest
  decode !base bs
    | ByteString.null bs = ([], base)
    | otherwise =
        let
          (delta, b1) = unvarint bs
          cyc = base + delta
          b2 = ByteString.unsafeTail b1
          (mv, b3) = case ByteString.unsafeHead b1 of
            2 -> (Nothing, b2)
            0 -> let (val, r) = ByteString.splitAt n b2 in (Just (0, fromBytes val), r)
            _ ->
              let (val, r1) = ByteString.splitAt n b2
                  (mask, r2) = ByteString.splitAt n r1
               in (Just (fromBytes mask, fromBytes val), r2)
          (rest, base') = decode cyc b3
         in
          (Change cyc mv : rest, base')
  fromBytes = ByteString.foldr' (\w acc -> acc `shiftL` 8 .|. fromIntegral w) 0

-- | A varint, and the bytes after it.
unvarint :: ByteString.ByteString -> (Int, ByteString.ByteString)
unvarint = go 0 0
 where
  go !acc !sh b =
    let w = ByteString.unsafeHead b
        acc' = acc .|. (fromIntegral (w .&. 0x7f) `shiftL` sh)
     in if w < 0x80
          then (acc', ByteString.unsafeTail b)
          else go acc' (sh + 7) (ByteString.unsafeTail b)

{- | Read a spill file, also one whose run was stopped or killed: every
signal with its records, and the lookup-table entries. A stopped run ends
where its furthest signal's records end. Reading stops at the first region
that is cut off or does not make sense. 'Left' when the file is not a spill
file.
-}
readSpill :: FilePath -> IO (Either String Recording)
readSpill path = bracket (openBinaryFile path ReadMode) hClose $ \h -> do
  size <- fromIntegral <$> hFileSize h
  header <- ByteString.hGet h 32
  case runParser ((,) <$> takeBytes 8 <*> byte) header of
    Just ((m, version), rest)
      | m /= magic -> pure (Left (path <> " is not a spill file"))
      | version /= spillVersion -> pure (Left (path <> " is a spill file of another version"))
      | Just ((start, first), _) <- runParser ((,) <$> (takeBytes 7 *> word64P) <*> word64P) rest ->
          regions h size start first Map.empty Map.empty [] Nothing
    _ -> pure (Left (path <> " is not a spill file"))
 where
  -- The signals are kept evaluated as each region is read: a lazy update
  -- would keep the region's bytes, and with them the whole file, in memory.
  -- @lasts@: the cycle of each signal's newest record so far.
  regions h size start !off !sigs !lasts luts end
    | off < 0 || off + headerBytes > size = done start sigs luts end
    | otherwise = do
        hSeek h AbsoluteSeek (fromIntegral off)
        hdr <- ByteString.hGet h headerBytes
        case runParser ((,,,) <$> word64P <*> word64P <*> word64P <*> word64P) hdr of
          Just ((rsize, owner, used, progress), _)
            | rsize >= headerBytes
            , used >= 0
            , used <= rsize - headerBytes
            , used <= size - off - headerBytes -> do
                payload <- ByteString.hGet h used
                let continue = regions h size start (off + rsize)
                if owner == 0
                  then case runParser (metadata sigs luts end) payload of
                    Just ((sigs', luts', end'), rest)
                      | ByteString.null rest -> continue sigs' lasts luts' end'
                    _ -> done start sigs luts end
                  else case Map.lookup (owner - 1) sigs of
                    Nothing -> continue sigs lasts luts end
                    Just s -> do
                      let i = owner - 1
                          base = Map.findWithDefault 0 i lasts
                      case lastRecord ((signalWidth s + 7) `div` 8) base payload of
                        Nothing -> done start sigs luts end
                        Just newest -> do
                          let !chunk = OnFile (off + headerBytes) used
                              !hash = ByteString.foldl' fnv (signalHash s) payload
                              !reached = maximum [signalEnd s, progress, if used > 0 then newest + 1 else 0]
                              !s' = s{signalEnd = reached, signalChunks = chunk : signalChunks s, signalHash = hash}
                          continue (Map.insert i s' sigs) (Map.insert i newest lasts) luts end
          _ -> done start sigs luts end
  done start sigs luts end = do
    let signals = [s{signalChunks = reverse (signalChunks s)} | s <- Map.elems sigs]
        reached = maximum (0 : [signalEnd s * signalPeriod s | s <- signals])
    pure (Right (Recording (Just path) signals start (fromMaybe reached end) (reverse luts)))

-- | The blocks of a metadata region, read into what was read before.
metadata ::
  Map.Map Int RecordedSignal ->
  [ByteString.ByteString] ->
  Maybe Int ->
  Parser (Map.Map Int RecordedSignal, [ByteString.ByteString], Maybe Int)
metadata !sigs luts end = Parser $ \b ->
  if ByteString.null b
    then Just ((sigs, luts, end), b)
    else
      runParser
        ( do
            tag <- byte
            len <- fromIntegral <$> word32P
            payload <- takeBytes len
            case runParser (block tag) payload of
              Just (Declared i sig, _) -> metadata (Map.insert i sig sigs) luts end
              Just (Lut, _) -> metadata sigs (ByteString.copy payload : luts) end
              Just (Ended e, _) -> metadata sigs luts (Just e)
              Just (BrokeDown i err, _) ->
                metadata (Map.adjust (\s -> s{signalDescription = Broken err}) i sigs) luts end
              Just (Unknown, _) -> metadata sigs luts end
              Nothing -> Parser (const Nothing)
        )
        b

{- | The cycle of the last of these records of a signal whose values take
this many bytes, the first counted from this cycle; 'Nothing' when they do
not make sense.
-}
lastRecord :: Int -> Int -> ByteString.ByteString -> Maybe Int
lastRecord n = go
 where
  go !base b
    | ByteString.null b = Just base
    | otherwise = case runParser ((,) <$> varintP <*> byte) b of
        Just ((delta, tag), r) ->
          let skip = case tag of
                2 -> 0
                0 -> n
                _ -> 2 * n
           in if ByteString.length r < skip then Nothing else go (base + delta) (ByteString.drop skip r)
        Nothing -> Nothing

-- | What a block of a spill file says.
data Block
  = Declared Int RecordedSignal
  | Lut
  | Ended Int
  | BrokeDown Int String
  | Unknown

-- | A block's payload, by its tag.
block :: Word8 -> Parser Block
block 1 = do
  i <- varintP
  kind <- (\k -> if k == 0 then TraceKind else ProbeKind) <$> byte
  period <- varintP
  width <- varintP
  key <- stringP
  desc <-
    byte >>= \d -> case d of
      1 -> Described <$> stringP <*> bytesP
      2 -> Broken <$> stringP
      _ -> pure Undescribed
  pure (Declared i (RecordedSignal key kind period width desc 0 [] 14695981039346656037))
block 3 = pure Lut
block 4 = Ended <$> varintP
block 5 = BrokeDown <$> varintP <*> stringP
block _ = pure Unknown

-- | A parser of bytes that fails on what it cannot read, rather than crash.
newtype Parser a = Parser {runParser :: ByteString.ByteString -> Maybe (a, ByteString.ByteString)}

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (\(a, r) -> (f a, r)) . p)

instance Applicative Parser where
  pure a = Parser (\b -> Just (a, b))
  Parser pf <*> Parser pa = Parser $ \b -> do
    (f, r) <- pf b
    (a, r') <- pa r
    pure (f a, r')

instance Monad Parser where
  Parser pa >>= k = Parser $ \b -> pa b >>= \(a, r) -> runParser (k a) r

byte :: Parser Word8
byte = Parser ByteString.uncons

takeBytes :: Int -> Parser ByteString.ByteString
takeBytes n = Parser $ \b ->
  if n >= 0 && ByteString.length b >= n then Just (ByteString.splitAt n b) else Nothing

-- | A little-endian number of this many bytes.
littleEndian :: Int -> Parser Word64
littleEndian k = ByteString.foldr' (\w acc -> acc `shiftL` 8 .|. fromIntegral w) 0 <$> takeBytes k

{- | A 64-bit number, as an 'Int': one beyond 'maxBound' is negative, which
every use of one refuses.
-}
word64P :: Parser Int
word64P = fromIntegral <$> littleEndian 8

word32P :: Parser Word64
word32P = littleEndian 4

varintP :: Parser Int
varintP = go 0 0
 where
  go acc sh
    | sh > 63 = Parser (const Nothing)
    | otherwise =
        byte >>= \w ->
          let acc' = acc .|. (fromIntegral (w .&. 0x7f) `shiftL` sh)
           in if w < 0x80 then pure acc' else go acc' (sh + 7)

bytesP :: Parser ByteString.ByteString
bytesP = varintP >>= takeBytes

stringP :: Parser String
stringP =
  bytesP >>= \b -> either (const (Parser (const Nothing))) (pure . Text.unpack) (Text.decodeUtf8' b)

{- | The VCD time unit of a recording, in ps: the greatest common divisor of
its signals' periods (1 when it has none).
-}
timescale :: Recording -> Int
timescale rec = case map signalPeriod (recordingSignals rec) of
  [] -> 1
  p : ps -> foldr gcd p ps

-- | A time in ps, in units of the 'timescale', rounded up.
ticks :: Recording -> Int -> Int
ticks rec ps = case ps `divMod` timescale rec of
  (q, 0) -> q
  -- No @ps + t - 1@: a window that ends at 'maxBound' must not overflow.
  (q, _) -> q + 1
