{-# LANGUAGE BangPatterns #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The recorder's storage: every recorded signal appends its value CHANGES to a
spill file while the simulation runs, so memory does not grow with the length
of a run.

Each signal encodes its changes into a small buffer of its own. The buffer
starts small and doubles as needed, so a signal that rarely changes costs a
few bytes; when it reaches 'chunkBytes', it is appended to the spill file as
one chunk, and the signal keeps only the chunk's offset and length. A change
record is the cycle (as the distance from the previous record, a varint), a
tag, and the value's bytes:

* tag 0: a defined value, @ceil (width / 8)@ bytes, little endian;
* tag 1: a value with undefined bits: the value's bytes, then the mask's;
* tag 2: a gap: from this cycle on, the signal was not evaluated (@z@).

Before its first record, a signal was not evaluated either.

A finished 'Recording' reads its changes back lazily, one chunk at a time
('readChanges'), so rendering does not hold a history in memory either.
-}
module Clash.CircuitContext.Internal.Recorder (
  -- * Values
  Value,

  -- * Recording
  Kind (..),
  Recorder,
  newRecorder,
  Sig,
  sigPeriod,
  newSig,
  recordCycle,
  recorderEnd,
  closeRecorder,

  -- * A finished recording
  Recording (..),
  RecordedSignal (..),
  Change (..),
  withReader,
  readChanges,
  timescale,
  ticks,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (bracket)
import Control.Monad (forM)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Word (Word64, Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (pokeByteOff)
import GHC.Natural (Natural)
import System.IO (
  Handle,
  IOMode (ReadMode, WriteMode),
  SeekMode (AbsoluteSeek),
  hClose,
  hPutBuf,
  hSeek,
  openBinaryFile,
 )
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString

import Clash.Shockwaves.Internal.Types (Translator, TypeName)

{- | A sample as the VCD writer needs it: @(mask, value)@, where a set mask
bit is an undefined bit.
-}
type Value = (Natural, Natural)

-- | A traced signal, or a probe inside a mealy machine's step function.
data Kind = TraceKind | ProbeKind
  deriving (Eq, Show)

-- | Where a running simulation records: one spill file, shared by its signals.
data Recorder = Recorder
  { rPath :: FilePath
  , rStart :: !Int
  -- ^ The time, in ps, from which on cycles are recorded.
  , rFile :: MVar (Maybe Handle, Int)
  {- ^ The spill file ('Nothing' once the recorder is closed), and the offset
  the next chunk is written at.
  -}
  , rSignals :: IORef [Sig]
  -- ^ Every signal registered, newest first.
  }

-- | One recorded signal, while the simulation runs.
data Sig = Sig
  { sigKey :: String
  , sigKind :: Kind
  , sigPeriod :: !Int
  , sigWidth :: !Int
  , sigAdt :: Maybe (TypeName, Translator)
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
  , ssChunks :: ![(Int, Int)]
  -- ^ The written chunks as @(offset, length)@, newest first.
  , ssHash :: !Word64
  -- ^ A hash of every record, to find signals with the same history.
  }

-- | The size at which a signal's buffer is appended to the spill file.
chunkBytes :: Int
chunkBytes = 16 * 1024

{- | A recorder that writes its spill file at this path, and records the
cycles in progress at or after this time (in ps). Truncates an existing file.
-}
newRecorder :: FilePath -> Int -> IO Recorder
newRecorder path start = do
  h <- openBinaryFile path WriteMode
  Recorder path start <$> newMVar (Just h, 0) <*> newIORef []

-- | Register a signal: its key, kind, clock period (ps), width and description.
newSig ::
  Recorder -> String -> Kind -> Int -> Int -> Maybe (TypeName, Translator) -> IO Sig
newSig rec key kind period width adt = do
  buf <- mallocForeignPtrBytes cap
  st <- newMVar (SigState 0 Nothing True 0 buf cap 0 [] 14695981039346656037)
  let sig = Sig key kind period width adt (rStart rec `div` period) st
  atomicModifyIORef' (rSignals rec) (\ss -> (sig : ss, ()))
  pure sig
 where
  -- Room for the two records one cycle can write (a gap, then a value).
  cap = 64 + 2 * recordBytes width

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
recordCycle rec sig cyc v = modifyMVar_ (sigState sig) step
 where
  step s
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
            else emit sig cyc (Just v) s1
        let s3 = s2{ssNext = cyc + 1}
        if ssLen s3 >= chunkBytes then flush rec s3 else pure s3

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

-- | Append the signal's buffer to the spill file as one chunk.
flush :: Recorder -> SigState -> IO SigState
flush rec s
  | ssLen s == 0 = pure s
  | otherwise = do
      off <- modifyMVar (rFile rec) $ \(mh, o) -> case mh of
        Just h -> do
          withForeignPtr (ssBuf s) $ \p -> hPutBuf h p (ssLen s)
          pure ((mh, o + ssLen s), Just o)
        -- Closed: this cycle came after the recording ended.
        Nothing -> pure ((mh, o), Nothing)
      pure
        s
          { ssLen = 0
          , ssChunks = maybe (ssChunks s) (\o -> (o, ssLen s) : ssChunks s) off
          }

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
  , signalAdt :: Maybe (TypeName, Translator)
  -- ^ The @clash-shockwaves@ description of the payload type, if it has one.
  , signalEnd :: Int
  -- ^ Cycles from this one on were not evaluated.
  , signalChunks :: [(Int, Int)]
  -- ^ Where its records are in the spill file, oldest first.
  , signalHash :: Word64
  }

-- | A finished recording: its spill file, and the signals recorded into it.
data Recording = Recording
  { recordingSpill :: FilePath
  , recordingSignals :: [RecordedSignal]
  -- ^ In registration order.
  , recordingStartPs :: Int
  -- ^ Cycles before the one in progress at this time (ps) were not recorded.
  , recordingEndPs :: Int
  -- ^ The time (ps) at which the simulation stopped: nothing was evaluated after it.
  }

{- | Finish a recording that ends at this time (ps): write every buffer and
close the spill file. A cycle recorded after this is dropped.
-}
closeRecorder :: Recorder -> Int -> IO Recording
closeRecorder rec end = do
  sigs <- reverse <$> readIORef (rSignals rec)
  frozen <- forM sigs $ \sig -> modifyMVar (sigState sig) $ \s0 -> do
    s <- flush rec s0
    pure
      ( s
      , RecordedSignal
          { signalKey = sigKey sig
          , signalKind = sigKind sig
          , signalPeriod = sigPeriod sig
          , signalWidth = sigWidth sig
          , signalAdt = sigAdt sig
          , signalEnd = ssNext s
          , signalChunks = reverse (ssChunks s)
          , signalHash = ssHash s
          }
      )
  modifyMVar_ (rFile rec) $ \(mh, o) -> mapM_ hClose mh >> pure (Nothing, o)
  pure (Recording (rPath rec) frozen (rStart rec) end)

-- | A record: from this cycle on, the signal has this value ('Nothing': not evaluated).
data Change = Change !Int !(Maybe Value)
  deriving (Eq, Show)

-- | Run an action with the recording's spill file open for 'readChanges'.
withReader :: Recording -> (Handle -> IO a) -> IO a
withReader rec = bracket (openBinaryFile (recordingSpill rec) ReadMode) hClose

{- | A signal's records, oldest first, read lazily: a chunk is read when the
list reaches it. The handle must stay open while the list is consumed, and
nothing else may move it in between (one thread).
-}
readChanges :: Handle -> RecordedSignal -> IO [Change]
readChanges h sig = go 0 (signalChunks sig)
 where
  n = (signalWidth sig + 7) `div` 8
  go _ [] = pure []
  go base ((off, len) : rest) = unsafeInterleaveIO $ do
    hSeek h AbsoluteSeek (fromIntegral off)
    bytes <- ByteString.hGet h len
    let (changes, base') = decode base bytes
    (changes <>) <$> go base' rest
  decode !base bytes
    | ByteString.null bytes = ([], base)
    | otherwise =
        let
          (delta, b1) = unvarint 0 0 bytes
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
  unvarint :: Int -> Int -> ByteString.ByteString -> (Int, ByteString.ByteString)
  unvarint !acc !sh b =
    let w = ByteString.unsafeHead b
        acc' = acc .|. (fromIntegral (w .&. 0x7f) `shiftL` sh)
     in if w < 0x80
          then (acc', ByteString.unsafeTail b)
          else unvarint acc' (sh + 7) (ByteString.unsafeTail b)
  fromBytes = ByteString.foldr' (\w acc -> acc `shiftL` 8 .|. fromIntegral w) 0

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
