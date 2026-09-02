{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

What the plugin says when it does not instrument something, and how you
control it.

Instrumentation falls back to identity rather than failing to compile — that
is what makes enabling the plugin package-wide safe, and it is also what lets
a wire go missing with nothing said. The fallback stays; the silence does
not. Every fallback is a real GHC diagnostic in a custom warning category, so
it is tuned with the flags every Haskell project already uses:

@
-Wno-x-circuit-context-untraced     -- I know, stop telling me
-Werror=x-circuit-context-untraced  -- strict: a lost wire fails the build
@

Four categories, because they differ in how likely each is to be a defect:

[@x-circuit-context@] The plugin could not honour something the source
  unambiguously asked for. Nothing benign lands here.

[@x-circuit-context-uninstrumented@] A signature asks for instrumentation
  somewhere the plugin does not reach, or does not scope: 'HasCircuitContext'
  without @OPAQUE@, @OPAQUE@ without a signature, both constraints at once,
  an instance or class-default body. Often intended (a small helper flattening
  into its caller), which is why it is its own category.

[@x-circuit-context-undecided@] The traceability oracle could not decide, and
  fell back to NOT tracing. This is the category that matters most: the oracle
  approximates instance solvability, and a decision it could not reach is a
  limitation of the approximation, not a fact about your types. Promote this
  one to an error on a design whose waveform you depend on.

[@x-circuit-context-untraced@] The oracle
  found no instance. A real answer, and the verbose one: it fires once per
  declined binding, including all the @Int@s and @Bool@s that were never going
  to trace.

On GHC 9.6 custom warning categories do not exist, so these are plain
warnings: still real diagnostics that @-Werror@ promotes wholesale, but not
individually silenceable. Per-category control starts at GHC 9.8.
-}
module Clash.CircuitContext.Plugin.Diagnostics (
  Category (..),
  categoryFlag,
  report,
  renderPlain,
) where

import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Errors.Types as GHC (TcRnMessage (TcRnUnknownMessage))
import qualified GHC.Tc.Types as GHC (TcM)
import qualified GHC.Tc.Utils.Monad as GHC (addDiagnosticAt)
import qualified GHC.Utils.Error as GHC (mkPlainDiagnostic)

#if __GLASGOW_HASKELL__ >= 908
import qualified GHC.Types.Error as GHC (mkSimpleUnknownDiagnostic)
import qualified GHC.Unit.Module.Warnings as GHC (mkWarningCategory)
#else
import qualified GHC.Types.Error as GHC (UnknownDiagnostic (UnknownDiagnostic))
#endif

-- | Which warning flag a message answers to. See the module header.
data Category
  = -- | @x-circuit-context@
    Unhonoured
  | -- | @x-circuit-context-uninstrumented@
    Uninstrumented
  | -- | @x-circuit-context-undecided@
    Undecided
  | -- | @x-circuit-context-untraced@
    Untraced
  deriving (Eq, Ord, Show)

categoryFlag :: Category -> String
categoryFlag = \case
  Unhonoured -> "x-circuit-context"
  Uninstrumented -> "x-circuit-context-uninstrumented"
  Undecided -> "x-circuit-context-undecided"
  Untraced -> "x-circuit-context-untraced"

{- | Emit one diagnostic, at most once per process.

The at-most-once is not cosmetic. The renamer pass runs twice when the plugin
is enabled twice, and the oracle is asked the same question once per
constraint-solving round per occurrence — so without it a single fallback
would be reported a dozen times, which reads exactly like the doubled output
that used to be a real bug. Messages carry their own source span, so "the
same message" is the same site.
-}
{- FOURMOLU_DISABLE -}
-- fourmolu cannot parse CPP inside a declaration; keep this region verbatim.
-- The wrapper around a plugin's own diagnostic gained and lost type
-- parameters across the supported GHCs, so it is applied at the one place
-- where 'TcRnUnknownMessage' fixes the type rather than named in a signature.
report :: GHC.SrcSpan -> Category -> [String] -> GHC.TcM ()
report spn cat body = do
  fresh <- GHC.liftIO (claim key)
  when fresh
    $ GHC.addDiagnosticAt spn
    $ GHC.TcRnUnknownMessage
#if __GLASGOW_HASKELL__ >= 908
      (GHC.mkSimpleUnknownDiagnostic diagnostic)
#else
      (GHC.UnknownDiagnostic diagnostic)
#endif
 where
  key = (renderPlain spn, categoryFlag cat, unlines body)
  diagnostic = GHC.mkPlainDiagnostic (reasonFor cat) [] doc
  doc = GHC.vcat (map GHC.text body)

#if __GLASGOW_HASKELL__ >= 908
reasonFor :: Category -> GHC.DiagnosticReason
reasonFor =
  GHC.WarningWithCategory . GHC.mkWarningCategory . GHC.fsLit . categoryFlag
#else
-- No custom warning categories before GHC 9.8: a plain warning, promoted by a
-- blanket -Werror but not silenceable on its own.
reasonFor :: Category -> GHC.DiagnosticReason
reasonFor _ = GHC.WarningWithoutFlag
#endif
{- FOURMOLU_ENABLE -}

{- | Render for a user-facing message: USER style, not the default dump
style, and no uniques — a skolem printed as @dom_ajlh[sk:1]@ is noise in a
message whose reader is looking at the source line it names.
-}
renderPlain :: (GHC.Outputable a) => a -> String
renderPlain =
  GHC.renderWithContext
    GHC.defaultSDocContext
      { GHC.sdocSuppressUniques = True
      , GHC.sdocStyle = GHC.defaultUserStyle
      }
    . GHC.ppr

-- | 'True' the first time this message is seen in this process.
claim :: (String, String, String) -> IO Bool
claim k =
  atomicModifyIORef' saidAlready $ \said ->
    (Set.insert k said, not (Set.member k said))

saidAlready :: IORef (Set.Set (String, String, String))
saidAlready = unsafePerformIO (newIORef Set.empty)
{-# NOINLINE saidAlready #-}
