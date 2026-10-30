{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

A hand-written 'BitPack' instance that compiles against every clash-prelude
this package supports.

clash-prelude's @master@ added the class method 'maybeUnpack', with a
default that needs a 'GHC.Generics.Generic' representation of the type. A
hand-written instance for a type without one (a newtype around 'Integer')
must define the method; on an older clash-prelude, defining it is an error,
because the method does not exist. Both carry the version number 1.11.0, so
CPP cannot tell them apart. 'withMaybeUnpack' asks the class itself instead.
-}
module Clash.CircuitContext.Internal.BitPackCompat (withMaybeUnpack) where

import Clash.Prelude (BitPack (unpack))
import Language.Haskell.TH

{- | The given instance declarations, each with @maybeUnpack = Just . unpack@
added when 'BitPack' has that method. Only for instances whose 'unpack'
accepts every bit pattern, which is what @Just . unpack@ claims (as
clash-prelude's own @Int64@ and @Word64@ instances do).
-}
withMaybeUnpack :: Q [Dec] -> Q [Dec]
withMaybeUnpack decs = do
  has <- classHasMethod "maybeUnpack"
  if has then map addMaybeUnpack <$> decs else decs
 where
  addMaybeUnpack (InstanceD overlap ctx ty body) =
    InstanceD overlap ctx ty (body <> [maybeUnpackD])
  addMaybeUnpack d = d

  maybeUnpackD =
    ValD
      (VarP (mkName "maybeUnpack"))
      (NormalB (InfixE (Just (ConE 'Just)) (VarE '(.)) (Just (VarE 'unpack))))
      []

  classHasMethod nm =
    reify ''BitPack >>= \case
      ClassI (ClassD _ _ _ _ methods) _ ->
        pure (any (isMethod nm) methods)
      _ -> pure False

  isMethod nm = \case
    SigD n _ -> nameBase n == nm
    _ -> False
