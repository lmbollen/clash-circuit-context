{-# LANGUAGE LambdaCase #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The plugin's command-line options, shared by both halves.

Instrumentation is deliberately silent: a binding the plugin cannot trace
falls back to identity rather than failing the build, which is what makes
enabling the plugin package-wide safe. The cost is that a MISSING wire looks
exactly like a wire that was never asked for. @diagnostics@ is the way to
tell the two apart — a build flag rather than a code change, so it can be
turned on for one compile without touching the design:

> cabal build --ghc-options='-fplugin-opt=Clash.CircuitContext.Plugin:diagnostics'
-}
module Clash.CircuitContext.Plugin.Options (
  Options (..),
  defaultOptions,
  parseOptions,
) where

-- | Everything @-fplugin-opt=Clash.CircuitContext.Plugin:\<opt\>@ can select.
newtype Options = Options
  { optDiagnostics :: Bool
  {- ^ @diagnostics@: report, on stderr, every decision that silently costs a
  wire or a scope — bindings whose payload type the oracle declined (with the
  requirement it got stuck on), 'Clash.CircuitContext.HasCircuitContext'
  functions without an @OPAQUE@ pragma, and signatures whose mode is not what
  they look like. Verbose by design: it lists every untraced binding, not only
  the surprising ones, because which one is surprising is exactly what the
  reader knows and the plugin does not.
  -}
  }
  deriving (Eq, Show)

-- | Silent, as when no @-fplugin-opt@ is given.
defaultOptions :: Options
defaultOptions = Options{optDiagnostics = False}

{- | Parse the @-fplugin-opt@ strings, returning the unrecognised ones
alongside. Callers report those: a mistyped flag that silently did nothing
is the same failure mode this module exists to remove.
-}
parseOptions :: [String] -> (Options, [String])
parseOptions = foldl step (defaultOptions, [])
 where
  step (opts, unknown) = \case
    "diagnostics" -> (opts{optDiagnostics = True}, unknown)
    other -> (opts, unknown <> [other])
