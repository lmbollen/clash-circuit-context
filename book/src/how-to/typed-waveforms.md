# How to show payload types in a waveform viewer

A VCD only has bits. This guide shows how to make a viewer show the value of
a signal as its type: `Busy 2`, not `0b0110`.

1. Give each payload type a `Waveform` instance from
   [clash-shockwaves](https://github.com/clash-lang/clash-shockwaves). For an
   algebraic data type, derive it beside `BitPack`:

   ```haskell,clash group=typed hidden
   {-# LANGUAGE OverloadedStrings #-}
   {-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}
   module Typed where

   import Clash.Explicit.Prelude
   import Clash.CircuitContext
   import Clash.CircuitContext.Waveform
   import Clash.Shockwaves.Waveform (Waveform)
   import qualified Data.Aeson as Json
   import qualified Data.Aeson.Key as Key
   import qualified Data.Aeson.KeyMap as KeyMap
   import qualified Prelude as P
   ```

   ```haskell,clash group=typed
   data Phase = Idle | Busy (Index 4) | Done
     deriving (Generic, NFDataX, BitPack, Waveform)

   sequencer :: (HasCircuitContext) => Signal System Bool -> Signal System Bool
   sequencer start = done
    where
     phase = register clockGen resetGen enableGen Idle next
     next = step <$> phase <*> start
     done = isDone <$> phase
     step Idle True = Busy 0
     step (Busy 3) _ = Done
     step (Busy n) _ = Busy (n + 1)
     step _ _ = Idle
     isDone Done = True
     isDone _ = False
   {-# OPAQUE sequencer #-}
   ```

2. Write the waveform with a function of `Clash.CircuitContext.Waveform`, as
   in [How to keep waveforms from a test suite](test-suite.md). Beside each
   `.vcd` file, it writes a `.json` file: the sidecar. The sidecar describes
   each signal whose payload type has a `Waveform` instance.

   ```haskell,clash group=typed
   record :: IO ()
   record = withWaveformSlot "sequencer" $ \slot -> do
     _ <- withWaveform slot 8 (sampleN 8 (sequencer (pure True)))
     pure ()

   -- | The signals that a sidecar describes.
   described :: FilePath -> IO [String]
   described path = do
     Just (Json.Object sidecar) <- Json.decodeFileStrict path
     case KeyMap.lookup "signals" sidecar of
       Just (Json.Object signals) -> pure (P.map Key.toString (KeyMap.keys signals))
       _ -> pure []

   >>> record
   >>> described "waveforms/sequencer.json"
   ["sequencer.done","sequencer.next","sequencer.phase"]
   ```

   The keys are the full paths of the signals in the VCD.

3. Open the `.vcd` file in [Surfer](https://surfer-project.org/) with the
   Surfer plugin of clash-shockwaves. The plugin reads the sidecar beside the
   VCD. See the clash-shockwaves README for how to install the plugin.

## If a type has no `Waveform` instance

The signal is still recorded, as bits. The plugin warns about it in the
category `x-circuit-context-undescribed`, with the type that needs an
instance. Put the instance in the package that defines the type. If bits are
what you want, turn the warning off: see [How to control the warnings of the
plugin](warnings.md).

If you write a `Waveform` instance by hand and its width does not agree with
`BitSize`, the sidecar does not describe that signal, and a message on
stderr names it. The VCD still has the signal, as bits.
