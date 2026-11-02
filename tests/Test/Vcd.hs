{-# LANGUAGE DataKinds #-}

{- | A small VCD decoder for the Test level: enough to hold a dumped (or
captured) waveform against the values a simulation computed.

Deliberately name-based: several @$var@ declarations may share one identifier
code (the alias-dedup encoding), and decoding each NAME independently is
exactly what makes an aliasing bug visible to a test — a code-based decoder
would follow the alias and hide it.
-}
module Test.Vcd (
  decodeVCD,
  decodeVCDPaths,
  bitsToInteger,
  isZ,
  asInts,
  model,
  cycleCount,
) where

import Clash.Prelude (Unsigned)
import qualified Data.Map.Strict as Map

{- | name → one bit string per cycle over @[0, n)@, values carried forward
between change lines.
-}
decodeVCD :: Int -> String -> Map.Map String [String]
decodeVCD n vcd = decodeWith n vcd [(nm, code) | (_, nm, code) <- declared vcd]

{- | 'decodeVCD', keyed by the full dotted path (@top.acc_0.total@) instead
of the bare name, for signals whose names repeat across scopes.
-}
decodeVCDPaths :: Int -> String -> Map.Map String [String]
decodeVCDPaths n vcd =
  decodeWith n vcd [(scopeDots p nm, code) | (p, nm, code) <- declared vcd]
 where
  scopeDots p nm = foldr (\sc acc -> sc <> "." <> acc) nm p

-- | Each @$var@ with the scopes around it (outermost first), name and code.
declared :: String -> [([String], String, String)]
declared = go [] . map words . lines
 where
  go st (["$scope", "module", sc, "$end"] : rest) = go (st <> [sc]) rest
  go st (["$upscope", "$end"] : rest) = go (take (length st - 1) st) rest
  go st (["$var", "wire", _w, code, nm, "$end"] : rest) = (st, nm, code) : go st rest
  go st (_ : rest) = go st rest
  go _ [] = []

decodeWith :: Int -> String -> [(String, String)] -> Map.Map String [String]
decodeWith n vcd vars = Map.fromList [(nm, waveFor code) | (nm, code) <- vars]
 where
  ls = lines vcd
  events = go (0 :: Int) ls
  go t (l : rest) = case l of
    '#' : num -> go (read num) rest
    'b' : _
      | [bits, code] <- words l -> (t, code, drop 1 bits) : go t rest
    _ -> go t rest
  go _ [] = []
  waveFor code =
    [ last ("z" : [bits | (t, c, bits) <- events, c == code, t <= cyc])
    | cyc <- [0 .. n - 1]
    ]

-- | 'Nothing' for a value with undefined (@x@) or never-sampled (@z@) bits.
bitsToInteger :: String -> Maybe Integer
bitsToInteger = foldl step (Just 0)
 where
  step acc ch = do
    v <- acc
    d <- case ch of
      '0' -> Just 0
      '1' -> Just 1
      _ -> Nothing
    pure (v * 2 + d)

-- | A cycle the recorder never retained (window-dropped or never forced).
isZ :: String -> Bool
isZ = all (== 'z')

-- | A decoded wave, as defined 'Integer's where possible.
asInts :: [String] -> [Maybe Integer]
asInts = map bitsToInteger

-- | What a faithful wave of these simulated values decodes to.
model :: [Unsigned 8] -> [Maybe Integer]
model = map (Just . toInteger)

-- | How many cycles a VCD contains (one @#@ timestamp per cycle).
cycleCount :: String -> Int
cycleCount vcd = length [() | '#' : _ <- lines vcd]
