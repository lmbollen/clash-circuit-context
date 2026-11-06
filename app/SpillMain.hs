{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The command line of @spill2vcd@ and @spill2fst@: write the waveform of spill
files, also of runs that were stopped. Each file is converted by
'Clash.CircuitContext.Recording.writeSpill', so the executables write what
the library writes.
-}
module SpillMain (spillMain, spillWith) where

import Control.Monad (foldM, when)
import Data.Char (isDigit)
import GHC.Natural (Natural)
import System.Console.GetOpt (
  ArgDescr (NoArg, ReqArg),
  ArgOrder (Permute),
  OptDescr (Option),
  getOpt,
  usageInfo,
 )
import System.Directory (removeFile)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath ((-<.>))
import System.IO (hPutStrLn, stderr)

import Clash.CircuitContext.Recording (Format (..), writeSpill)

data Options = Options
  { optOutput :: Maybe FilePath
  , optWindow :: Maybe (Natural, Natural)
  , optDelete :: Bool
  , optHelp :: Bool
  }

options :: [OptDescr (Options -> Either String Options)]
options =
  [ Option
      "o"
      ["output"]
      (ReqArg (\f o -> Right o{optOutput = Just f}) "FILE")
      "the waveform's path (one SPILL only);\nby default SPILL with the waveform's\nextension"
  , Option
      "w"
      ["window"]
      (ReqArg (\w o -> (\win -> o{optWindow = Just win}) <$> window w) "START,STOP")
      "the window to show, [START, STOP), in\nunits of the VCD timescale: the\ngreatest common divisor of the clock\nperiods (for a design in one domain:\ncycles), for FST too; by default all\nthe run recorded"
  , Option
      ""
      ["delete"]
      (NoArg (\o -> Right o{optDelete = True}))
      "delete each SPILL once its waveform is\nwritten"
  , Option "h" ["help"] (NoArg (\o -> Right o{optHelp = True})) "show this help"
  ]
 where
  window w = case break (== ',') w of
    (a, ',' : b) | number a, number b, read a < (read b :: Natural) -> Right (read a, read b)
    _ -> Left ("the window " <> w <> " is not START,STOP with 0 <= START < STOP")
  number x = not (null x) && all isDigit x

-- | Run the command line, writing waveforms of this format, and exit.
spillMain :: Format -> IO ()
spillMain fmt = do
  prog <- getProgName
  args <- getArgs
  spillWith fmt prog args >>= exitWith

{- | The command line of a program of this name, with these arguments: what
'spillMain' runs, returning the exit code instead of exiting.
-}
spillWith :: Format -> String -> [String] -> IO ExitCode
spillWith fmt prog args = do
  let
    header =
      "usage: "
        <> prog
        <> " [OPTION]... SPILL...\n\nWrite the "
        <> show fmt
        <> " waveform of each spill file of clash-circuit-context, and\n"
        <> "its .json sidecar beside it. The spill file of a run that was stopped\n"
        <> "holds what the run recorded up to there.\n"
    usage = usageInfo header options
    failWith msg = do
      hPutStrLn stderr (prog <> ": " <> msg)
      hPutStrLn stderr ("Try '" <> prog <> " --help' for more information.")
      pure (ExitFailure 1)
  case getOpt Permute options args of
    (fs, spills, []) -> case foldM (flip ($)) (Options Nothing Nothing False False) fs of
      Left err -> failWith err
      Right opts
        | optHelp opts -> putStr usage >> pure ExitSuccess
        | null spills -> failWith "no spill file given"
        | length spills > 1
        , Just _ <- optOutput opts ->
            failWith "--output takes one spill file"
        | otherwise -> do
            results <- mapM (convert opts) spills
            pure (if and results then ExitSuccess else ExitFailure 1)
    (_, _, errs) -> failWith (concatMap (takeWhile (/= '\n')) errs)
 where
  convert opts spill = do
    let out = maybe (spill -<.> extension) id (optOutput opts)
    written <- writeSpill fmt (optWindow opts) spill out
    case written of
      Left err -> hPutStrLn stderr (prog <> ": " <> spill <> ": " <> err) >> pure False
      Right (start, stop) -> do
        when (optDelete opts) (removeFile spill)
        putStrLn (prog <> ": wrote " <> out <> ", window " <> show start <> "," <> show stop)
        pure True
  extension = case fmt of
    VCD -> "vcd"
    FST -> "fst"
