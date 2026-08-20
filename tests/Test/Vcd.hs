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
decodeVCD n vcd = Map.fromList [(nm, waveFor code) | (nm, code) <- vars]
 where
  ls = lines vcd
  vars =
    [ (nm, code)
    | l <- ls
    , ["$var", "wire", _w, code, nm, "$end"] <- [words l]
    ]
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
