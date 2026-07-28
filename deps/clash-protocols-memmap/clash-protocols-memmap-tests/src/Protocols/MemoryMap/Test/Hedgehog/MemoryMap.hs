-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Tools to help build tests for memory maps
module Protocols.MemoryMap.Test.Hedgehog.MemoryMap where

import Clash.Explicit.Prelude

import qualified Data.List as L
import qualified Data.Map.Strict as Map

import Protocols.MemoryMap (
  Address,
  MemoryMap (..),
  Register (..), DeviceDefinition (..), DeviceDefinitions, Path, MemoryMapTreeAnn (AnnInterconnect, AnnDeviceInstance), NamedLoc,
 )
import Protocols.MemoryMap.Check.AbsAddress (MemoryMapTreeAbsNorm, AbsNormData (..))
import Protocols.MemoryMap.TypeDescription.TH
import qualified Language.Haskell.TH as TH

-- | One instance of a register in a memory map
--
-- If one device type has multiple instances in a memory map,
-- each register will be emitted for each instance.
data TraversedRegister = TraversedRegister
  { regDesc :: NamedLoc Register
  , instancePath :: Path
  , instanceAddr :: Address
  , deviceDef :: DeviceDefinition
  }

-- | Traverse a memory map and output a list of all registers present
traverseRegisters :: MemoryMap -> MemoryMapTreeAbsNorm -> [TraversedRegister]
traverseRegisters mm = traverseTree mm.deviceDefs
 where
  traverseTree :: DeviceDefinitions -> MemoryMapTreeAbsNorm -> [TraversedRegister]
  traverseTree defs (AnnInterconnect _ _ (fmap snd -> comps)) = L.concatMap (traverseTree defs) comps
  traverseTree defs (AnnDeviceInstance absData _ name) = findRegs absData (defs Map.! name)

  findRegs :: AbsNormData -> DeviceDefinition -> [TraversedRegister]
  findRegs AbsNormData{path, absoluteAddr} devDef = flip fmap devDef.registers $ \reg ->
    TraversedRegister { regDesc = reg, instancePath = path, instanceAddr = absoluteAddr, deviceDef = devDef }

-- | Representation of a type of a register that allows more flexible
-- tests by allowing access to compile time variable
data RegisterTestType where
  RTTBool :: RegisterTestType
  RTTFloat :: RegisterTestType
  RTTDouble :: RegisterTestType
  RTTBitVector :: SNat n -> RegisterTestType
  RTTUnsigned :: SNat n -> RegisterTestType
  RTTSigned :: SNat n -> RegisterTestType
  RTTIndex :: SNat n -> RegisterTestType
  RTTVec :: SNat n -> RegisterTestType -> RegisterTestType
  RTTTuple :: [RegisterTestType] -> RegisterTestType
  RTTOther :: TypeRef -> RegisterTestType

-- | Compute the 'RegisterTestType' of a 'TypeRef'
typeRefToRegTestType :: TypeRef -> RegisterTestType
typeRefToRegTestType (TupleType _ args) = RTTTuple $ typeRefToRegTestType <$> args
typeRefToRegTestType (Variable _) = error ""
typeRefToRegTestType (TypeNat _) = error ""
typeRefToRegTestType tyRef@(TypeInst name args)
  | Just action <- lookup name builtins = action args
  | otherwise = RTTOther tyRef
 where
  builtins :: [(TH.Name, [TypeRef] -> RegisterTestType)]
  builtins =
   [ (''Bool, const RTTBool)
   , (''Float, const RTTFloat)
   , (''Double, const RTTDouble)
   , (''BitVector, (`withArgSnat` RTTBitVector))
   , (''Unsigned, (`withArgSnat` RTTUnsigned))
   , (''Signed, (`withArgSnat` RTTSigned))
   , (''Index, (`withArgSnat` RTTIndex))
   , (''Vec, \tyArgs -> withVecArgSnat tyArgs $ \snat ty -> RTTVec snat (typeRefToRegTestType ty))
   ]

  withArgSnat :: [TypeRef] -> (forall (n :: Nat). SNat n -> a) -> a
  withArgSnat [TypeNat n] action = withSomeSNat n $ \case
      Just snat -> action (withKnownNat snat $ snatProxy snat)
      Nothing -> error ""
  withArgSnat _ _ = error ""

  withVecArgSnat :: [TypeRef] -> (forall (n :: Nat). SNat n -> TypeRef -> a) -> a
  withVecArgSnat [TypeNat n, ty] action = withSomeSNat n $ \case
      Just snat -> action (withKnownNat snat $ snatProxy snat) ty
      Nothing -> error ""
  withVecArgSnat _ _ = error ""
