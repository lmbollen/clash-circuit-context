-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Functions for statefully simulating Wishbone transactions
module Protocols.MemoryMap.Test.Hedgehog.WbTransaction
  ( TestDone
  , TestInstruction (..)
  , TransactionT
  , Transaction
  , TransactionResult
  , read
  , write
  , testSucceeded
  , testFailed
  , idle
  , assert
  , runWbTest
  ) where

import Clash.Explicit.Prelude hiding (assert, read, singleton)

import Clash.Class.BitPackC

import Control.Monad.Operational

import Data.Constraint (Dict (..))
import Debug.Trace

import Protocols (Circuit (..), toSignals)
import Protocols.Experimental.Wishbone
import Protocols.MemoryMap (
  Address,
 )

import Control.Arrow.Transformer.Automaton (Automaton(Automaton))
import Text.Printf (printf)
import Clash.Class.BitPackC.Words (packWordC, maybeUnpackWordC)
import Control.Monad (forM_, forM)
import Data.Functor.Identity (Identity)
import Unsafe.Coerce (unsafeCoerce)
import Clash.Sized.Vector.ToTuple (VecToTuple(vecToTuple))
import Data.Tuple (Solo(MkSolo))

-- | Marker type to indicate that a test is done.
data TestDone = TestDone

-- | Possible instructions needed to test wishbone circuits
--
-- This is hardcoded to use 4 byte words.
-- This could be changed in the future but made setting this up easier to start.
data TestInstruction a where
  -- | Reading a word from an address using SEL
  ReadFrom :: Address -> BitVector 4 -> TestInstruction (WishboneS2MResult (BitVector 32))
  -- | Writing a word to an address with SEL
  WriteTo :: Address -> BitVector 4 -> BitVector 32 -> TestInstruction (WishboneS2MResult ())
  -- | Do nothing for a cycle
  Idle :: TestInstruction ()
  -- | Signal the framework that the test succeeded
  TestSucceeded :: TestInstruction TestDone
  -- | Signal to the framework that the test failed with an error message
  TestFailed :: String -> TestInstruction a

-- | Type for implementing a transaction-test within an underlying 'Monad' 'm'
type TransactionT m a = ProgramT TestInstruction m a
-- | Type for implementing a transaction test within the identity 'Monad'
type Transaction a = TransactionT Identity a

-- | Signal that the test is done and succeeded!
testSucceeded :: TransactionT m TestDone
testSucceeded = singleton TestSucceeded

-- | Signal that the test failed with a message
testFailed :: String -> TransactionT m a
testFailed msg = singleton $ TestFailed msg

-- | Do nothing for one cycle
idle :: TransactionT m ()
idle = singleton Idle

-- | Assert that a condition is true, failing the test with a message otherwise
assert :: (Monad m) => Bool -> String -> TransactionT m ()
assert True _ = pure ()
assert False msg = testFailed $ printf "Assertion failed: %s" msg

-- | Read any BitPackC value
--
-- At the moment, this only works with values that either
--   - have a size less or equal to a word and are contained completely within a read word
--   - have a size larger than a word and the value is expected to start at the word boundary
read ::
  forall a m.
  (BitPack a, BitPackC a, Monad m, ?byteOrder :: ByteOrder) =>
  Address ->
  TransactionT m a
read addr = do
  case compareSNat (SNat @(ByteSizeC a)) (SNat @4) of
    SNatLE -> readLessThanWord addr
    _ -> do
      let align = natToInteger @(AlignmentBoundaryC a)
      if (addr `mod` align) /= 0 then
        testFailed $ printf "Unaligned read at addr %08X (expected alignment %i)" (toInteger addr) align
      else
        readAligned addr

-- | Write any 'BitPackC' value to an address.
--
-- At the moment, this only works with values that either
--   - have a size less or equal to a word and are contained completely within a word
--   - have a size larger than a word and the value is expected to start at the word boundary
write ::
  forall a m.
  (BitPackC a, Monad m, ?byteOrder :: ByteOrder) =>
  Address ->
  a ->
  TransactionT m ()
write addr val = do
  case compareSNat (SNat @(ByteSizeC a)) (SNat @4) of
    SNatLE -> writeLessThanWord addr val
    _ -> do
      let align = natToInteger @(AlignmentBoundaryC a)
      if (addr `mod` align) /= 0 then
        testFailed $ printf "Unaligned read at addr %08X (expected alignment %i)" (toInteger addr) align
      else
        writeAligned addr val

-- | Result of a transaction test
data TransactionResult
  = Success
  -- ^ Test ran successfully
  | Failure String
  -- ^ Test resulted in a failure, contains an error message
  deriving (Show)

-- | Run a list of Wishbone transaction tests.
--
-- Takes a circuit to be used for testing/simulation and a list of test cases.
--
-- This runs all tests one after another using the same circuit, so any state changes
-- performed in a previous test case could be observable in following tests.
runWbTest ::
  forall addrWidth dom m.
  (KnownNat addrWidth, 1 <= addrWidth, KnownDomain dom, Monad m) =>
  Circuit (Wishbone dom 'Standard addrWidth 4) () ->
  [(String, TransactionT m TestDone)] ->
  m [(String, TransactionResult)]
runWbTest circuit = runAutomaton (signalAutomaton fn)
 where
  fn = toInputOutput $ toSignals circuit

  toInputOutput :: ((a, ()) -> (b, ())) -> (a -> b)
  toInputOutput f input = fst $ f (input, ())

  runAutomaton :: TestAutomaton addrWidth 4 -> [(String, TransactionT m TestDone)] -> m [(String, TransactionResult)]
  runAutomaton _ [] = pure []
  runAutomaton automaton0 ((name, transaction):rest) = do
    instrView <- viewT transaction
    (automaton1, res) <- runTransaction automaton0 name instrView
    restResults <- runAutomaton automaton1 rest
    pure $ (name, res) : restResults

  runTransaction ::
    TestAutomaton addrWidth 4 ->
    String ->
    ProgramViewT TestInstruction m TestDone ->
    m (TestAutomaton addrWidth 4, TransactionResult)
  runTransaction automaton0@(Automaton step0) testName instructions = case instructions of
    Return TestDone -> pure (automaton0, Success)
    TestSucceeded :>>= _ -> pure (automaton0, Success)
    TestFailed msg :>>= _ -> pure (automaton0, Failure msg)
    Idle :>>= next -> do
      let (_s2m, automaton1) = step0 emptyWishboneM2S
      let nextInstr = next ()
      nextView <- viewT nextInstr
      runTransaction automaton1 testName nextView
    ReadFrom addr sel :>>= next -> do
      (automaton1, res) <- readData addr sel automaton0
      let nextInstr = next res
      nextView <- viewT nextInstr
      runTransaction automaton1 testName nextView
    WriteTo addr sel val :>>= next -> do
      (automaton1, res) <- writeData addr sel val automaton0
      let nextInstr = next res
      nextView <- viewT nextInstr
      runTransaction automaton1 testName nextView

  readData :: Address -> BitVector 4 -> TestAutomaton addrWidth 4 -> m (TestAutomaton addrWidth 4, WishboneS2MResult (BitVector 32))
  readData addr sel (Automaton step0) = waitForResult (step0 m2s)
   where
    m2s = (emptyWishboneM2S @addrWidth) { addr = fromInteger addr, busSelect = sel, busCycle = True, strobe = True, writeEnable = False }

    waitForResult (s2m, automaton@(Automaton step))
     | s2m.acknowledge = pure (automaton, Acknowlege s2m.readData)
     | s2m.err = pure (automaton, Error)
     | s2m.retry = pure (automaton, Retry)
     | otherwise = waitForResult (step m2s)

  writeData :: Address -> BitVector 4 -> BitVector 32 -> TestAutomaton addrWidth 4 -> m (TestAutomaton addrWidth 4, WishboneS2MResult ())
  writeData addr sel val (Automaton step0) = waitForResult (step0 m2s)
   where
    m2s = (emptyWishboneM2S @addrWidth @4) { addr = fromInteger addr, busSelect = sel, busCycle = True, strobe = True, writeEnable = True, writeData = val }

    waitForResult (s2m, automaton@(Automaton step))
     | s2m.acknowledge = pure (automaton, Acknowlege ())
     | s2m.err = pure (automaton, Error)
     | s2m.retry = pure (automaton, Retry)
     | otherwise = waitForResult (step m2s)

data WishboneS2MResult a
  = Acknowlege a
  | Error
  | Retry

type TestAutomaton addrWidth n = Automaton (->) (WishboneM2S addrWidth n) (WishboneS2M n)

-- | Perform a raw read, returning all possible Wishbone results
--
-- See 'readRaw' for a version that expects the result to be an ACK
readRaw' :: Address -> BitVector 4 -> TransactionT m (WishboneS2MResult (BitVector 32))
readRaw' addr sel = singleton $ ReadFrom addr sel

-- | Perform a raw write, returning all possible Wishbon results
--
-- See 'readRaw' for a version that expects the result to be an ACK
writeRaw' :: Address -> BitVector 4 -> BitVector 32 -> TransactionT m (WishboneS2MResult ())
writeRaw' addr sel val = singleton $ WriteTo addr sel val

-- | Perform a raw read request, failing the test when the Wishbone reply is anything but ACK
readRaw :: (Monad m) => Address -> BitVector 4 -> TransactionT m (BitVector 32)
readRaw addr sel = do
  res <- readRaw' addr sel
  case res of
    Acknowlege val -> pure val
    Error -> testFailed $ printf "Bus signalled ERR on read (%X, %X)" (toInteger addr) (toInteger sel)
    Retry -> testFailed $ printf "Bus signalled RETRY on read (%X, %X)" (toInteger addr) (toInteger sel)

-- | Perform a raw write request, failing the test when the Wishbone reply is anything but ACK
writeRaw :: (Monad m) => Address -> BitVector 4 -> BitVector 32 -> TransactionT m ()
writeRaw addr sel val = do
  res <- writeRaw' addr sel val
  case res of
    Acknowlege () -> pure ()
    Error -> testFailed $ printf "Bus signalled ERR on write (%X, %X) %X" (toInteger addr) (toInteger sel) (toInteger val)
    Retry -> testFailed $ printf "Bus signalled RETRY on write (%X, %X) %X" (toInteger addr) (toInteger sel) (toInteger val)

{-| Words spit out are in ?regByteOrder -}
readNWordsFromAligned ::
  forall n m. (Monad m) =>
  SNat n ->
  Address ->
  TransactionT m (Vec n (BitVector 32))
readNWordsFromAligned SNat addr = do
  let wordIndices = iterateI @n (+1) 0
  let wordAlignedAddr = addr `div` 4
  forM wordIndices $ \i -> do
    let currentWordAddr = wordAlignedAddr + i
    readRaw currentWordAddr 0b1111

readAligned ::
  forall a m.
  (BitPack a, BitPackC a, Monad m, ?byteOrder :: ByteOrder) =>
  Address ->
  TransactionT m a
readAligned addr = do
  -- output is in regByteOrder
  words0 <- readNWordsFromAligned SNat addr

  let x = maybeUnpackWordC ?byteOrder words0
  case x of
    Nothing -> testFailed $ printf "Error when trying to unpack value from memory at %X" addr
    Just val -> do
      pure val

readLessThanWord ::
  forall a m.
  (BitPackC a, Monad m, ?byteOrder :: ByteOrder) =>
  (ByteSizeC a <= 4) =>
  Address ->
  TransactionT m a
readLessThanWord addr = do
  let size = natToInteger @(ByteSizeC a)
  let subWordAddr = addr `mod` 4
  let wordAlignedAddr = addr `div` 4 -- subWordAddr
  word0 <- readRaw wordAlignedAddr 0b1111
  traceM $ printf "raw word: %08X" (toInteger word0)
  let word1 = extractDataFromWord word0 size subWordAddr
  case cancelMulDiv @(ByteSizeC a) @4 of
    Dict -> case maybeUnpackWordC ?byteOrder (word1 :> Nil) of
     Nothing -> testFailed $ printf "Error when trying to unpack value from memory at %X" addr
     Just val -> do
       pure val

writeAligned ::
  forall a m.
  (BitPackC a, Monad m, ?byteOrder :: ByteOrder) =>
  Address ->
  a ->
  TransactionT m ()
writeAligned addr val = do
  let wordAlignedAddr = addr `div` 4
  let bytes = natToInteger @(ByteSizeC a)
  let x = packWordC @4 ?byteOrder val
  forM_ (iterateI (+ 1) 0 `zip` x) $ \(i, word0) -> do
    let bytesSoFar = i * 4
    let bytesLeft = bytes - bytesSoFar
    let bytesInWord = bytesLeft `min` 4
    let offset = 0
    let (word1, sel) = packageDataIntoWord word0 bytesInWord offset
    writeRaw (wordAlignedAddr + i) sel word1

writeLessThanWord ::
  forall a m.
  (BitPackC a, Monad m, ?byteOrder :: ByteOrder) =>
  (ByteSizeC a <= 4) =>
  Address ->
  a ->
  TransactionT m ()
writeLessThanWord addr val = do
  let size = natToInteger @(ByteSizeC a)
  let subWordAddr = addr `mod` 4
  let wordAlignedAddr = addr `div` 4
  case cancelMulDiv @(ByteSizeC a) @4 of
    Dict -> do
      let (vecToTuple -> (MkSolo word0)) = packWordC @4 ?byteOrder val
      let (word1, sel) = packageDataIntoWord word0 size subWordAddr
      writeRaw wordAlignedAddr sel word1

-- | Takes word in native order (BE) and outputs word in native order
extractDataFromWord :: BitVector 32 -> Integer -> Integer -> BitVector 32
extractDataFromWord word size offset =
    case (size, offset, unpack word :: Vec 4 (BitVector 8)) of
      (1, 0, _ :> _ :> _ :> a :> Nil) -> pack $ 0 :> 0 :> 0 :> a :> Nil
      (1, 1, _ :> _ :> a :> _ :> Nil) -> pack $ 0 :> 0 :> 0 :> a :> Nil
      (1, 2, _ :> a :> _ :> _ :> Nil) -> pack $ 0 :> 0 :> 0 :> a :> Nil
      (1, 3, a :> _ :> _ :> _ :> Nil) -> pack $ 0 :> 0 :> 0 :> a :> Nil
      (2, 0, _ :> _ :> a :> b :> Nil) -> pack $ 0 :> 0 :> a :> b :> Nil
      (2, 1, _ :> a :> b :> _ :> Nil) -> pack $ 0 :> 0 :> a :> b :> Nil
      (2, 2, a :> b :> _ :> _ :> Nil) -> pack $ 0 :> 0 :> a :> b :> Nil
      (3, 0, _ :> a :> b :> c :> Nil) -> pack $ 0 :> a :> b :> c :> Nil
      (3, 1, a :> b :> c :> _ :> Nil) -> pack $ 0 :> a :> b :> c :> Nil
      _ -> word

{-| Incoming word is native (big endian) byte order, outputs in native byte order -}
packageDataIntoWord ::
  BitVector 32 ->
  Integer ->
  Integer ->
  (BitVector 32, BitVector 4)
packageDataIntoWord word size offset =
    case (size, offset, unpack word :: Vec 4 (BitVector 8)) of
      (1, 0, _ :> _ :> _ :> a :> Nil) -> (pack $ 0 :> 0 :> 0 :> a :> Nil, 0b0001)
      (1, 1, _ :> _ :> _ :> a :> Nil) -> (pack $ 0 :> 0 :> a :> 0 :> Nil, 0b0010)
      (1, 2, _ :> _ :> _ :> a :> Nil) -> (pack $ 0 :> a :> 0 :> 0 :> Nil, 0b0100)
      (1, 3, _ :> _ :> _ :> a :> Nil) -> (pack $ a :> 0 :> 0 :> 0 :> Nil, 0b1000)
      (2, 0, _ :> _ :> a :> b :> Nil) -> (pack $ 0 :> 0 :> a :> b :> Nil, 0b0011)
      (2, 1, _ :> _ :> a :> b :> Nil) -> (pack $ 0 :> a :> b :> 0 :> Nil, 0b0110)
      (2, 2, _ :> _ :> a :> b :> Nil) -> (pack $ a :> b :> 0 :> 0 :> Nil, 0b1100)
      (3, 0, _ :> a :> b :> c :> Nil) -> (pack $ 0 :> a :> b :> c :> Nil, 0b0111)
      (3, 1, _ :> a :> b :> c :> Nil) -> (pack $ a :> b :> c :> 0 :> Nil, 0b1110)
      _ -> (word, 0b1111)


cancelMulDiv :: forall a b. (1 <= b, a <= b) => Dict (DivRU a b ~ 1)
cancelMulDiv = unsafeCoerce (Dict :: Dict (0 ~ 0))
