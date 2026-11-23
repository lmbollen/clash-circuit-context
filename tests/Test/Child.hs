{- | Tests that run in a process of their own: a test that can crash the
process, that changes what the whole process may do (such as the size of the
files it may write), or that measures the process (its stderr, its memory).

The test executable runs itself again, with @CCC_TEST_CHILD@ set to the name
of the test; "Main" then runs that test's child instead of the suite. A child
passes when it exits normally: a failed assertion, an exception or a crash
fails it.
-}
module Test.Child (
  Child,
  childMain,
  inChild,
  assertChild,
) where

import Control.Monad (unless)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (ExitSuccess), die)
import System.Process (env, proc, readCreateProcessWithExitCode)

import Test.Tasty.HUnit (Assertion, assertFailure)

-- | A child, by name: what it runs in its own process.
type Child = (String, IO ())

variable :: String
variable = "CCC_TEST_CHILD"

{- | When this process is a child, run the child it is for, of these, and
'True'; else 'False'.
-}
childMain :: [Child] -> IO Bool
childMain children = do
  which <- lookupEnv variable
  case which of
    Nothing -> pure False
    Just name -> do
      maybe (die ("no child test named " <> name)) id (lookup name children)
      pure True

{- | Run the child of this name, with these RTS options. Returns how it
exited, and what it wrote to stdout and stderr.
-}
inChild :: String -> [String] -> IO (ExitCode, String, String)
inChild name rts = do
  exe <- getExecutablePath
  parent <- getEnvironment
  let args = if null rts then [] else "+RTS" : rts <> ["-RTS"]
  readCreateProcessWithExitCode
    (proc exe args){env = Just ((variable, name) : filter ((/= variable) . fst) parent)}
    ""

-- | The child of this name, with these RTS options, passes.
assertChild :: String -> [String] -> Assertion
assertChild name rts = do
  (code, out, err) <- inChild name rts
  unless (code == ExitSuccess) $
    assertFailure (name <> " exited with " <> show code <> ":\n" <> out <> err)
