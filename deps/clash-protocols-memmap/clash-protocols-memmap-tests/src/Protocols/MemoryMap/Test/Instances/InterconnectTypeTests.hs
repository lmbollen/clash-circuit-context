-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
module Protocols.MemoryMap.Test.Instances.InterconnectTypeTests where

import Clash.Prelude hiding (read)

import GHC.Stack (HasCallStack)
import Protocols (Circuit)
import Protocols.Experimental.Wishbone (Wishbone, WishboneMode(..))
import Protocols.MemoryMap
import Protocols.MemoryMap.Registers.WishboneStandard (deviceWbI, deviceConfig, registerConfig, registerWbI_)
import Clash.Class.BitPackC (ByteOrder)
import Protocols.MemoryMap.Test.Interconnect (interconnect)

import Protocols.MemoryMap.Test.Hedgehog.WbTransaction
import Text.Printf (printf)
import Protocols.MemoryMap.Check (runMakeAbsolute)
import Protocols.MemoryMap.Test.Hedgehog.MemoryMap

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))

mm :: (?byteOrder :: ByteOrder) => MemoryMap
mm = getMMAny $ withClockResetEnable @System clockGen resetGen enableGen interconnectTypeTests

interconnectTypeTests ::
  forall dom.
  (HasCallStack, HiddenClockResetEnable dom, HasCallStack) =>
  (?byteOrder :: ByteOrder) =>
  Circuit (ToConstBwd Mm, Wishbone dom 'Standard 8 4) ()
interconnectTypeTests = circuit $ \(mm, master) -> do
  -- [prim] <- interconnect -< (mm, master)
  [primA, primB] <- withName "Primitives" interconnect -< (mm, master) --prim
  withName "A" registerPrimitivesTest -< primA
  withName "B" registerPrimitivesTest -< primB

registerPrimitivesTest ::
  (HasCallStack, HiddenClockResetEnable dom, KnownNat addrWidth) =>
  (?byteOrder :: ByteOrder) =>
  Circuit (ToConstBwd Mm, Wishbone dom 'Standard addrWidth 4) ()
registerPrimitivesTest = circuit $ \(mm, wb) -> do
  [   bv8_1
    , bv8_42
    , bv8_128
    , bv8_38
    , bv15_1
    , bv15_42
    , bv15_128
    , bv15_14348
    , bv8_arr4
    , bv15_arr2
    , bv64_42
    , bv48
    ] <- deviceWbI (deviceConfig "RegisterPrimitivesTest") -< (mm, wb)


  registerWbI_ (registerConfig "bv8_1" "") (1 :: BitVector 8) -< (bv8_1 , Fwd noWrite)
  registerWbI_ (registerConfig "bv8_42" "") (42 :: BitVector 8) -< (bv8_42, Fwd noWrite)
  registerWbI_ (registerConfig "bv8_128" "") (128 :: BitVector 8) -< (bv8_128, Fwd noWrite)
  registerWbI_ (registerConfig "bv8_38" "") (38 :: BitVector 8) -< (bv8_38, Fwd noWrite)

  registerWbI_ (registerConfig "bv15_1" "") (1 :: BitVector 15) -< (bv15_1, Fwd noWrite)
  registerWbI_ (registerConfig "bv15_42" "") (42 :: BitVector 15) -< (bv15_42, Fwd noWrite)
  registerWbI_ (registerConfig "bv15_128" "") (128 :: BitVector 15) -< (bv15_128, Fwd noWrite)
  registerWbI_ (registerConfig "bv15_14348" "") (14348 :: BitVector 15) -< (bv15_14348, Fwd noWrite)

  registerWbI_ (registerConfig "bv8_arr4" "") ((12 :> 34 :> 56 :> 78 :> Nil) :: Vec 4 (BitVector 8)) -< (bv8_arr4, Fwd noWrite)

  registerWbI_ (registerConfig "bv15_arr2" "") ((1234 :> 56789 :> Nil) :: Vec 2 (BitVector 15)) -< (bv15_arr2, Fwd noWrite)

  registerWbI_ (registerConfig "bv64_42" "") (0x4242_3535_9595_4848 :: BitVector 64) -< (bv64_42, Fwd noWrite)
  registerWbI_ (registerConfig "bv48" "") (0x4242_3535_9595 :: BitVector 48) -< (bv48, Fwd noWrite)
  where
    noWrite = pure Nothing



bv8Test ::
  (MonadIO m, ?byteOrder :: ByteOrder) =>
  Address ->
  BitVector 8 ->
  TransactionT m TestDone
bv8Test addr val = do
  liftIO $ putStrLn "Testing BitVector 8"
  res0 <- read addr
  liftIO $ printf "Found value %X at address %X\n" (toInteger res0) (toInteger addr)
  assert (res0 == val) $ "intial value should be " <> show val <> " but found " <> show res0
  liftIO $ printf "Writing value %X\n" (toInteger $ complement res0)
  write addr (complement res0)
  res1 <- read addr
  liftIO $ printf "Found new value %X\n" (toInteger res1)
  assert (res1 == complement res0) "should complement"
  testSucceeded

bv15Test ::
  (MonadIO m, ?byteOrder :: ByteOrder) =>
  Address ->
  BitVector 15 ->
  TransactionT m TestDone
bv15Test addr val = do
  liftIO $ putStrLn "Testing BitVector 15"
  res0 <- read addr
  liftIO $ printf "Found value %X at address %X\n" (toInteger res0) (toInteger addr)
  assert (res0 == val) $ "intial value should be " <> show val <> " but found " <> show res0
  liftIO $ printf "Writing value %X\n" (toInteger $ complement res0)
  write addr (complement res0)
  res1  <- read addr
  liftIO $ printf "Found new value %X\n" (toInteger res1)
  assert (res1 == complement res0) "should complement"
  testSucceeded

bv64Test ::
  (MonadIO m, ?byteOrder :: ByteOrder) =>
  Address ->
  BitVector 64 ->
  TransactionT m TestDone
bv64Test addr val = do
  liftIO $ putStrLn "Testing BitVector 64"
  res0 <- read addr
  liftIO $ printf "Found value %X at address %X\n" (toInteger res0) (toInteger addr)
  assert (res0 == val) $ "intial value should be " <> show val <> " but found " <> show res0
  liftIO $ printf "Writing value %X\n" (toInteger $ complement res0)
  write addr (complement res0)
  res1  <- read addr
  liftIO $ printf "Found new value %X\n" (toInteger res1)
  assert (res1 == complement res0) "should complement"
  testSucceeded

runInterconnectTypeTests :: (?byteOrder :: ByteOrder) => IO [(String, TransactionResult)]
runInterconnectTypeTests = runWbTest circuit1 testList
 where
  testList =
    [
      ("A bv8_1", bv8Test 0x00 1)
    , ("A bv8_42", bv8Test 0x04 42)
    , ("A bv8_128", bv8Test 0x08 128)
    , ("A bv8_38", bv8Test 0x0C 38)
    , ("B bv8_1", bv8Test (512 + 0x00) 1)
    , ("B bv8_42", bv8Test (512 + 0x04) 42)
    , ("B bv8_128", bv8Test (512 + 0x08) 128)
    , ("B bv8_38", bv8Test (512 + 0x0C) 38)


    , ("A bv15_1", bv15Test 0x10 1)
    , ("A bv15_42", bv15Test 0x14 42)
    , ("A bv15_128", bv15Test 0x18 128)
    , ("A bv15_14348", bv15Test 0x1C 14348)
    , ("B bv15_1", bv15Test (512 + 0x10) 1)
    , ("B bv15_42", bv15Test (512 + 0x14) 42)
    , ("B bv15_128", bv15Test (512 + 0x18) 128)
    , ("B bv15_14348", bv15Test (512 + 0x1C) 14348)

    , ("A bv8_arr4_1", bv8Test 0x20 12)
    , ("A bv8_arr4_2", bv8Test 0x21 34)
    , ("A bv8_arr4_3", bv8Test 0x22 56)
    , ("A bv8_arr4_4", bv8Test 0x23 78)

    , ("A bv15_arr2_1", bv15Test 0x24 1234)
    , ("A bv15_arr2_2", bv15Test 0x26 56789)

    , ("A bv64_42", bv64Test 0x28 0x4242_3535_9595_4848)
    ]
    <> regTestList
  circuit0 = withClockResetEnable @System clockGen resetGen enableGen interconnectTypeTests
  circuit1 = unMemmap circuit0




regTestList :: forall m. (MonadIO m, ?byteOrder :: ByteOrder) => [(String, TransactionT m TestDone)]
regTestList = -- filter (\(name, _) -> "bv48" `isInfixOf` name) $
  makeTest <$> regs
 where
  relTree = convert mm.tree
  normTree = normalizeRelTree relTree
  (absTree, _) = runMakeAbsolute mm.deviceDefs (0x00, 0xFF) normTree

  regs = traverseRegisters mm absTree

  makeTest :: TraversedRegister -> (String, TransactionT m TestDone)
  makeTest reg = (testName, testFn regTestType regSize regAddr)
   where
    regSize = regByteSizeC reg.regDesc.value.fieldType
    testName = show reg.instancePath <> reg.deviceDef.deviceName.name <> "::" <> reg.regDesc.name.name

    typeRef = regFieldType reg.regDesc.value.fieldType

    regTestType = typeRefToRegTestType typeRef

    regAddr = reg.instanceAddr + reg.regDesc.value.address

    testFn rtt size addr = do
      case rtt of
        RTTBool -> testSucceeded
        RTTBitVector n@SNat -> bvFn n addr
        RTTVec n@SNat inner -> vecFn n inner size addr
        _ -> testSucceeded

    bvFn :: forall n. SNat n -> Address -> TransactionT m TestDone
    bvFn nSnat@SNat addr = do
      liftIO $ printf "BitVector %i at address %X\n" (snatToInteger nSnat) (toInteger addr)
      bv :: BitVector n <- read addr
      let complemented = complement bv
      write addr complemented
      bv1 <- read addr
      assert (bv1 == complement bv) "should complement"
      testSucceeded

    vecFn :: forall n. SNat n -> RegisterTestType -> Integer -> Address -> TransactionT m TestDone
    vecFn nSnat@SNat inner size addr = do
      let n = snatToInteger nSnat
      let innerSize = size `div` n
      forM_ [0..n] $ \i -> do
        testFn inner innerSize (addr + (i * innerSize))
      testSucceeded
