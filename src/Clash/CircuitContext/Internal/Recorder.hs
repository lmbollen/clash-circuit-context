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

Each signal encodes its changes into a small buffer of its own. The buffer
starts small and doubles as needed, so a signal that rarely changes costs a
few bytes; when it reaches 'chunkBytes', it becomes one chunk: appended to
the spill file, or kept in memory. A change record is the cycle (as the
distance from the previous record, a varint), a tag, and the value's bytes:

* tag 0: a defined value, @ceil (width / 8)@ bytes, little endian;
* tag 1: a value with undefined bits: the value's bytes, then the mask's;
* tag 2: a gap: from this cycle on, the signal was not evaluated (@z@).

Before its first record, a signal was not evaluated either.

== The spill file

A spill file describes itself, so that the file of a run that was stopped
can still be read ('readSpill'). It is a header, then blocks:

* header: the 8 bytes @CCCSPILL@, a version byte (1), and the time (ps,
  64 bits, little endian) from which cycles are recorded;
* block: a tag byte, the payload's length (32 bits, little endian), the
  payload. Numbers in a payload are varints, strings are a varint length and
  UTF-8 bytes.

  * 1, a signal: its id, kind (0 trace, 1 probe), clock period (ps), width,
    key, and description: 0 none; 1 described, with the type name and the
    types as JSON; 2 broken, with the error;
  * 2, a chunk: the signal's id, the cycle up to which the signal had
    recorded (exclusive), and change records;
  * 3, lookup-table entries, as JSON (a @clash-shockwaves@ @LUTMap@);
  * 4, the end: the time (ps) at which the simulation stopped;
  * 5, a signal's description broke after it registered: its id, and the
    error.

A signal's block comes before its chunks, and a block is written whole, so
any prefix of the file that ends at a block's end can be read: what the
signals had recorded up to their last chunks. A block cut off by a crash is
ignored.

A finished 'Recording' reads its changes back lazily, one chunk at a time
('readChanges'), so rendering does not hold a history in memory either.

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
  fromException,
  onException,
  try,
  uninterruptibleMask_,
 )
import Control.Monad (forM, unless)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Word (Word64, Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (pokeByteOff)
import GHC.Natural (Natural)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (
  BufferMode (NoBuffering),
  Handle,
  IOMode (ReadMode, WriteMode),
  SeekMode (AbsoluteSeek),
  hClose,
  hFileSize,
  hPutBuf,
  hSeek,
  hSetBuffering,
  openBinaryFile,
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

-- | Where a recorder's chunks go.
data Store
  = -- | The spill file, and the offset of its end.
    ToFile !Handle !Int
  | ToMemory
  | -- | The recording ended: a chunk after this is dropped.
    Closed

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
  , ssBuf :: !(ForeignPtr Word8)
  , ssCap :: !Int
  , ssLen :: !Int
  , ssChunks :: ![Chunk]
  -- ^ The finished chunks, newest first.
  , ssHash :: !Word64
  -- ^ A hash of every record, to find signals with the same history.
  , ssSeen :: !(Set.Set Value)
  -- ^ The values whose lookup-table entries are made ('sigLut' only).
  , ssBroken :: !(Maybe String)
  -- ^ Making a lookup-table entry threw this error.
  }

-- | A chunk of a signal's records.
data Chunk
  = -- | At this offset of the spill file, this many bytes.
    OnFile !Int !Int
  | InMemory !ByteString.ByteString

-- | The size at which a signal's buffer becomes a chunk.
chunkBytes :: Int
chunkBytes = 16 * 1024

-- | The first bytes of a spill file.
magic :: ByteString.ByteString
magic = ByteStringChar8.pack "CCCSPILL"

spillVersion :: Word8
spillVersion = 1

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
      h <- openBinaryFile path WriteMode
      -- Each block is written whole, so that a stopped run leaves whole blocks.
      hSetBuffering h NoBuffering
      let header =
            Builder.byteString magic
              <> Builder.word8 spillVersion
              <> Builder.word64LE (fromIntegral start)
      ByteStringLazy.hPut h (Builder.toLazyByteString header)
      pure (ToFile h (ByteString.length magic + 9))
  Recorder spill start
    <$> newMVar store
    <*> newIORef []
    <*> newIORef []
    <*> newIORef 0

-- | The recorder's spill file, if it has one.
recorderSpill :: Recorder -> Maybe FilePath
recorderSpill = rSpill

{- | Append a block to the spill file: this tag, this payload, and then these
raw bytes (part of the payload). Returns where the raw bytes start in the
file; 'Nothing' when there is no spill file, or the recording ended.

A block is written whole, also when the run is cancelled while it is written:
a block cut off in the middle, with more blocks after it, would make the rest
of the file unreadable.
-}
writeBlock ::
  Recorder -> Word8 -> Builder.Builder -> (Int, Raw -> IO ()) -> IO (Maybe Int)
writeBlock rec tag payload (rawLen, putRaw) =
  uninterruptibleMask_ $ modifyMVar (rStore rec) $ \store -> case store of
    ToFile h o -> do
      let
        body = ByteStringLazy.toStrict (Builder.toLazyByteString payload)
        len = ByteString.length body + rawLen
        hdr = Builder.word8 tag <> Builder.word32LE (fromIntegral len)
      ByteStringLazy.hPut h (Builder.toLazyByteString (hdr <> Builder.byteString body))
      putRaw (Raw h)
      let raw = o + 5 + ByteString.length body
      pure (ToFile h (raw + rawLen), Just raw)
    _ -> pure (store, Nothing)

-- | The spill file's handle, for the raw bytes of a 'writeBlock'.
newtype Raw = Raw Handle

-- | A block that is only its payload.
noRaw :: (Int, Raw -> IO ())
noRaw = (0, \_ -> pure ())

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
  buf <- mallocForeignPtrBytes cap
  st <-
    newMVar
      (SigState 0 Nothing True 0 buf cap 0 [] 14695981039346656037 Set.empty Nothing)
  let sig = Sig i key kind period width desc lut (rStart rec `div` period) st
  _ <-
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
      noRaw
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
  -- can be interrupted; the state is then as it was.
  s <- takeMVar (sigState sig)
  s' <- step restore s `onException` putMVar (sigState sig) s
  putMVar (sigState sig) s'
 where
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
        if ssLen s3 >= chunkBytes then flush rec sig s3 else pure s3

-- | Append one record to the signal's buffer, growing it when needed.
emit :: Sig -> Int -> Maybe Value -> SigState -> IO SigState
emit sig c mv s0 = do
  s <- reserve s0
  (o, h) <- withForeignPtr (ssBuf s) $ \p -> do
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
  reserve s
    | ssLen s + recordBytes (sigWidth sig) <= ssCap s = pure s
    | otherwise = do
        let cap' = 2 * ssCap s
        buf' <- mallocForeignPtrBytes cap'
        withForeignPtr (ssBuf s) $ \p ->
          withForeignPtr buf' $ \p' -> copyBytes p' p (ssLen s)
        pure s{ssBuf = buf', ssCap = cap'}

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
              () <$ writeBlock rec 3 (Builder.byteString json) noRaw
            pure s{ssSeen = Set.insert v (ssSeen s)}
          Left err -> do
            _ <- writeBlock rec 5 (varint (sigId sig) <> string err) noRaw
            pure s{ssBroken = Just err}
  _ -> pure s

-- | Make the signal's buffer a chunk: on the spill file, or in memory.
flush :: Recorder -> Sig -> SigState -> IO SigState
flush rec sig s
  | ssLen s == 0 = pure s
  | otherwise = do
      store <- readMVar (rStore rec)
      chunk <- case store of
        ToMemory ->
          Just . InMemory
            <$> withForeignPtr
              (ssBuf s)
              (\p -> ByteString.create (ssLen s) (\q -> copyBytes q p (ssLen s)))
        _ -> do
          at <-
            writeBlock
              rec
              2
              (varint (sigId sig) <> varint (ssNext s))
              (ssLen s, \(Raw h) -> withForeignPtr (ssBuf s) (\p -> hPutBuf h p (ssLen s)))
          pure ((`OnFile` ssLen s) <$> at)
      pure s{ssLen = 0, ssChunks = maybe (ssChunks s) (: ssChunks s) chunk}

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

{- | Finish a recording that ends at this time (ps): make every buffer a
chunk, mark the end in the spill file, and close it. A cycle recorded after
this is dropped.
-}
closeRecorder :: Recorder -> Int -> IO Recording
closeRecorder rec end = do
  sigs <- reverse <$> readIORef (rSignals rec)
  frozen <- forM sigs $ \sig -> modifyMVar (sigState sig) $ \s0 -> do
    s <- flush rec sig s0
    pure
      ( s
      , RecordedSignal
          { signalKey = sigKey sig
          , signalKind = sigKind sig
          , signalPeriod = sigPeriod sig
          , signalWidth = sigWidth sig
          , signalDescription = maybe (sigDescription sig) Broken (ssBroken s)
          , signalEnd = ssNext s
          , signalChunks = reverse (ssChunks s)
          , signalHash = ssHash s
          }
      )
  _ <- writeBlock rec 4 (varint end) noRaw
  modifyMVar_ (rStore rec) $ \store -> case store of
    ToFile h _ -> hClose h >> pure Closed
    _ -> pure Closed
  luts <- reverse <$> readIORef (rLuts rec)
  pure (Recording (rSpill rec) frozen (rStart rec) end luts)

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

{- | Read a spill file, also one whose run was stopped: every signal with
the records of its whole chunks, and the lookup-table entries. A stopped run
ends where its furthest signal's chunks end. Reading stops at the first block
that is cut off or does not make sense. 'Left' when the file is not a spill
file.
-}
readSpill :: FilePath -> IO (Either String Recording)
readSpill path = bracket (openBinaryFile path ReadMode) hClose $ \h -> do
  size <- fromIntegral <$> hFileSize h
  header <- ByteString.hGet h (ByteString.length magic + 9)
  case runParser ((,,) <$> takeBytes 8 <*> byte <*> takeBytes 8) header of
    Just ((m, version, start), _)
      | m /= magic -> pure (Left (path <> " is not a spill file"))
      | version /= spillVersion -> pure (Left (path <> " is a spill file of another version"))
      | otherwise ->
          blocks
            h
            size
            (fromIntegral (littleEndian start))
            (ByteString.length header)
            Map.empty
            []
            Nothing
    Nothing -> pure (Left (path <> " is not a spill file"))
 where
  blocks h size start off sigs luts end = do
    hdr <- ByteString.hGet h 5
    case runParser ((,) <$> byte <*> takeBytes 4) hdr of
      Just ((tag, lenBytes), _)
        | len <- fromIntegral (littleEndian lenBytes)
        , len <= size - off - 5 -> do
            payload <- ByteString.hGet h len
            let off' = off + 5 + len
                continue = blocks h size start off'
            case (tag, runParser (block tag) payload) of
              (_, Nothing) -> done start sigs luts end
              (_, Just (Declared i sig, _)) -> continue (Map.insert i sig sigs) luts end
              (_, Just (Chunked i next, records)) ->
                let at = off + 5 + (len - ByteString.length records)
                    add s =
                      s
                        { signalEnd = next
                        , signalChunks = OnFile at (ByteString.length records) : signalChunks s
                        , signalHash = ByteString.foldl' fnv (signalHash s) records
                        }
                 in continue (Map.adjust add i sigs) luts end
              (_, Just (Lut, _)) -> continue sigs (payload : luts) end
              (_, Just (Ended e, _)) -> continue sigs luts (Just e)
              (_, Just (BrokeDown i err, _)) ->
                continue (Map.adjust (\s -> s{signalDescription = Broken err}) i sigs) luts end
              (_, Just (Unknown, _)) -> continue sigs luts end
      _ -> done start sigs luts end
  done start sigs luts end = do
    let signals = [s{signalChunks = reverse (signalChunks s)} | s <- Map.elems sigs]
        reached = maximum (0 : [signalEnd s * signalPeriod s | s <- signals])
    pure (Right (Recording (Just path) signals start (fromMaybe reached end) (reverse luts)))
  littleEndian :: ByteString.ByteString -> Word64
  littleEndian = ByteString.foldr' (\w acc -> acc `shiftL` 8 .|. fromIntegral w) 0

-- | What a block of a spill file says.
data Block
  = Declared Int RecordedSignal
  | {- | A chunk's signal and the cycle up to which it had recorded; its
    records are what is left of the payload.
    -}
    Chunked Int Int
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
block 2 = Chunked <$> varintP <*> varintP
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
