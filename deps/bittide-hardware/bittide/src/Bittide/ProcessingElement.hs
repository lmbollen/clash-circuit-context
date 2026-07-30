-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}
-- Orphan 'Waveform' instances for clash-vexriscv's bus types (see below).
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.ProcessingElement where

import Clash.Explicit.Prelude hiding (delay)
import Clash.CircuitContext (HasCircuitContext)
import Clash.Prelude
import Clash.Shockwaves.Waveform (Waveform)
import Clash.Sized.Vector.Extra (incrementWithBlacklist)

import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.Experimental.Wishbone
import Protocols.Experimental.Wishbone.Orphans ()
import Protocols.Idle
import VexRiscv (CpuIn (..), CpuOut (..), DumpVcd, Jtag, JtagIn (..), JtagOut (..))

import Bittide.Cpus.Types (BittideCpu)
import Bittide.DoubleBufferedRam
import Bittide.SharedTypes
import Bittide.Wishbone

import Clash.Class.BitPackC (ByteOrder (BigEndian, LittleEndian))
import Clash.Cores.Xilinx.Ila (Depth (D32768, D4096))
import qualified VexRiscv.Reset as MinReset

import qualified Data.ByteString as BS
import qualified Protocols.MemoryMap as Mm (
  Mm,
  withDeviceTag,
  withTag,
 )
import qualified Protocols.ToConst as ToConst
import qualified Protocols.Vec as Vec

{- | clash-vexriscv is an external pin (not part of this instrumented tree),
so its CPU and JTAG bus types get their typed-waveform instances here, next
to the component whose traced bindings carry them ('rvCircuit''s @cpuOut@\/
@rvIn@\/@jtagIn@). Orphans by necessity; every DUT imports this module
transitively.
-}
deriving anyclass instance Waveform JtagIn

deriving anyclass instance Waveform JtagOut

deriving anyclass instance Waveform CpuIn

deriving anyclass instance Waveform CpuOut

-- | Configuration for a Bittide Processing Element.
data PeConfig nBusses where
  PeConfig ::
    forall depthI depthD nBusses iBusTimeout dBusTimeout.
    ( KnownNat depthI
    , 1 <= depthI
    , KnownNat depthD
    , 1 <= depthD
    , KnownNat nBusses
    , 2 <= nBusses
    , PrefixWidth nBusses <= 30
    ) =>
    { depthI :: SNat depthI
    -- ^ Depth of the instruction memory in number of 32-bit words.
    , depthD :: SNat depthD
    -- ^ Depth of the data memory in number of 32-bit words.
    , initI :: Maybe (ContentType depthI (Bytes 4))
    -- ^ Initial content of the instruction memory, can be smaller than its total depth.
    , initD :: Maybe (ContentType depthD (Bytes 4))
    -- ^ Initial content of the data memory, can be smaller than its total depth.
    , iBusTimeout :: SNat iBusTimeout
    {- ^ Number of clock cycles after which the a transaction on the instruction bus times out.
    Set to 0 to disable timeouts on the instruction bus.
    -}
    , dBusTimeout :: SNat dBusTimeout
    {- ^ Number of clock cycles after which the a transaction on the data bus times out.
    Set to 0 to disable timeouts on the data bus.
    -}
    , includeIlaWb :: Bool
    {- ^ Indicates whether or not to include the Wishbone ILA component probes. Should be set
    to 'False' if this CPU is not in an always-on domain. Additionally, only one CPU in any
    given system should have this set to 'True' in order to avoid probe name conflicts.
    -}
    , cpu :: forall dom. BittideCpu dom
    -- ^ The CPU to use in this processing element.
    } ->
    PeConfig nBusses

type PrefixWidth nBusses = CLog 2 (nBusses + 1)
type RemainingBusWidth nBusses = 30 - PrefixWidth nBusses

type PeInternalBusses = 2

{- | VexRiscV based RV32IMC core together with instruction memory, data memory and
'singleMasterInterconnect'.
-}
{-# OPAQUE processingElement #-}
processingElement ::
  forall dom nBusses pfxWidth.
  ( HasCircuitContext
  , HasCallStack
  , HiddenClockResetEnable dom
  , ?byteOrder :: ByteOrder
  , KnownNat nBusses
  , PeInternalBusses <= nBusses
  , KnownNat pfxWidth
  , pfxWidth <= 30
  , pfxWidth ~ PrefixWidth nBusses
  ) =>
  DumpVcd ->
  PeConfig nBusses ->
  Circuit
    (ToConstBwd Mm.Mm, Jtag dom)
    ( Vec
        (nBusses - PeInternalBusses)
        (BitboneMm dom (RemainingBusWidth nBusses))
    )
processingElement dumpVcd PeConfig{depthI, depthD, initI, initD, iBusTimeout, dBusTimeout, includeIlaWb, cpu} = circuit $ \(mm, jtagIn) -> do
  (iBus0, (mmDbus, dBus0)) <-
    rvCircuit cpu dumpVcd (pure low) (pure low) (pure low) -< (mm, jtagIn)
  iBus1 <-
    maybeIlaWb
      False
      (SSymbol @"instructionBus")
      2
      D4096
      onRequestWb
      onRequestWb
      -< iBus0
  dBus1 <-
    watchDogWb "dBus" dBusTimeout
      <| maybeIlaWb
        includeIlaWb
        (SSymbol @"dataBus")
        2
        D32768
        onRequestWb
        onRequestWb
      -< dBus0
  (pfxs, wbs) <- Vec.unzip <| singleMasterInterconnectC -< (mmDbus, dBus1)
  idleSink <| (Vec.vecCircuits $ fmap ToConst.toBwd prefixes) -< pfxs
  ([(mmI, iMemBus), (mmD, dMemBus)], extBusses) <- Vec.split -< wbs

  -- Instruction and data memory are never accessed explicitly by developers,
  -- only implicitly by the CPU itself. We therefore don't need to generate HAL
  -- code. We instruct the generator to skip them by adding a "no-generate" tag.
  --
  -- Waveforms: 'wbStorage' is pure circuit-notation wiring, but its port
  -- binders (and this block's, e.g. @dMemBus@/@iBus3@) auto-trace via
  -- clash-circuit-context's Tagged/tuple 'Traceable' instances, so the memory
  -- bus traffic shows up without any explicit taps.
  Mm.withTag "no-generate"
    $ Mm.withDeviceTag "no-generate"
    $ wbStorage "DataMemory" depthD initD
    -< (mmD, dMemBus)

  iBus2 <- removeMsb <| watchDogWb "iBus" iBusTimeout -< iBus1 -- XXX: <= This should be handled by an interconnect
  Mm.withTag "no-generate"
    $ Mm.withDeviceTag "no-generate"
    $ wbStorage "InstructionMemory" depthI initI
    -< (mmI, iBus3)
  iBus3 <- arbiter -< [iMemBus, iBus2]

  idC -< extBusses
 where
  -- We use `init` to generate at least 0 prefixes with `incrementWithBlacklist`
  prefixes = iMemPfx :> (incrementWithBlacklist @(nBusses - 1) prefixBlacklist)
  -- We dont want to use address 0 or the address with only the MSB set as prefixes.
  -- Address 0 is not used because it is often used as a null pointer.
  -- The address with only the MSB set is not used because we use it for the instruction
  -- memory
  iMemPfx = rotateR 1 1
  prefixBlacklist = 0 :> iMemPfx :> Nil
  removeMsb ::
    forall aw dw.
    (KnownNat aw) =>
    Circuit
      (Wishbone dom 'Standard (aw + pfxWidth) dw)
      (Wishbone dom 'Standard aw dw)
  removeMsb = wbMap (mapAddr (truncateB :: BitVector (aw + pfxWidth) -> BitVector aw)) id

  wbMap fwd bwd = Circuit $ \(m2s, s2m) -> (fmap bwd s2m, fmap fwd m2s)

{-# OPAQUE rvCircuit #-}
rvCircuit ::
  ( HasCircuitContext
  , HiddenClockResetEnable dom
  , ?byteOrder :: ByteOrder
  ) =>
  BittideCpu dom ->
  DumpVcd ->
  Signal dom Bit ->
  Signal dom Bit ->
  Signal dom Bit ->
  Circuit
    (ToConstBwd Mm.Mm, Jtag dom)
    ( Bitbone dom 30
    , BitboneMm dom 30
    )
rvCircuit cpu dumpVcd tInterrupt sInterrupt eInterrupt =
  case ?byteOrder of
    LittleEndian -> Circuit go
    BigEndian ->
      clashCompileError
        [i|
          Unsupported register byte order:

            BigEndian

          The only supported mode is:

            LittleEndian
        |]
 where
  go (((), jtagIn), (iBusIn, (mm, dBusIn))) = ((mm, jtagOut), (iBusWbM2S <$> cpuOut, ((), dBusWbM2S <$> cpuOut)))
   where
    tupToCoreIn (timerInterrupt, softwareInterrupt, externalInterrupt, iBusWbS2M, dBusWbS2M) =
      CpuIn{timerInterrupt, softwareInterrupt, externalInterrupt, iBusWbS2M, dBusWbS2M}
    rvIn = tupToCoreIn <$> bundle (tInterrupt, sInterrupt, eInterrupt, iBusIn, dBusIn)
    (cpuOut, jtagOut) = cpu dumpVcd hasClock rv32Reset rvIn jtagIn
    rv32Reset = MinReset.toMinCycles hasClock $ unsafeOrReset hasReset jtagReset
    jtagReset = unsafeFromActiveHigh (delay False (bitToBool . ndmreset <$> cpuOut))

-- | Map a function over the address field of 'WishboneM2S'
mapAddr ::
  (BitVector aw1 -> BitVector aw2) ->
  WishboneM2S aw1 selWidth ->
  WishboneM2S aw2 selWidth
mapAddr f wb = wb{addr = f (addr wb)}

{- | Provide a vector of filepaths, and a write operations containing a byteSelect and
a vector of characters and, for each filepath write the corresponding byte to that file
if the corresponding byteSelect is @1@.
-}
printCharacters ::
  (KnownNat paths, KnownNat chars, (paths + n) ~ chars) =>
  -- | Destination files for received bytes.
  Vec paths FilePath ->
  -- | Write attempt, bytes will only be written if the corresponding byteSelect is @1@.
  Maybe (BitVector chars, Vec chars Byte) ->
  IO ()
printCharacters Nil _ = pure ()
printCharacters paths@(Cons _ _) inps = case inps of
  Just (byteSelect, chars) ->
    sequence_ $ printToFiles <*> take SNat (unpack byteSelect) <*> take SNat chars
  Nothing -> pure ()
 where
  printToFiles = printToFile <$> paths
  printToFile path byteSelect char
    | byteSelect = BS.appendFile path $ BS.singleton $ bitCoerce char
    | otherwise = pure ()
