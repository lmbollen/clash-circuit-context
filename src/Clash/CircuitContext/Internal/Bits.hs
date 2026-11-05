{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

A recorded value as the text VCD and FST both write: one character per bit.
-}
module Clash.CircuitContext.Internal.Bits (bits) where

import Data.Bits (shiftR, testBit)
import Data.Char (ord)
import Data.Word (Word64, Word8)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Storable (pokeByteOff)
import GHC.Natural (Natural)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString (unsafeCreate)

{- | A value's bits, most significant first: 0/1 defined, @x@ evaluated but
undefined, @z@ not evaluated. Filled 64 bits at a time from machine words:
this is most of the work of writing a VCD.
-}
bits :: Int -> Maybe (Natural, Natural) -> ByteString.ByteString
bits w mv = ByteString.unsafeCreate w $ \p -> case mv of
  Nothing -> fillBytes p (c2w 'z') w
  Just (mask, val) -> mapM_ (word p mask val) [0 .. (w - 1) `div` 64]
 where
  word p mask val k = do
    let
      m = fromIntegral (mask `shiftR` (64 * k)) :: Word64
      v = fromIntegral (val `shiftR` (64 * k)) :: Word64
      digit d
        | testBit m d = c2w 'x'
        | testBit v d = c2w '1'
        | otherwise = c2w '0'
    mapM_
      (\d -> pokeByteOff p (w - 1 - (64 * k + d)) (digit d))
      [0 .. min 63 (w - 1 - 64 * k)]
  c2w :: Char -> Word8
  c2w = fromIntegral . ord
