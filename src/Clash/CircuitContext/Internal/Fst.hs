{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

A binding to the FST writer of GTKWave's @libfst@ (vendored in
@cbits\/libfst@, MIT). Only what the waveform writer here uses: scopes, wire
vars, and value changes as @0@/@1@/@x@/@z@ text, most significant bit first;
and, to tell a failing test's waveform from one written on purpose, its
comment ('hasComment').

Times are in ps. The writer requires them to go up: a value for a time before
the last 'timeChange' cannot be written.
-}
module Clash.CircuitContext.Internal.Fst (
  Writer,
  Handle,
  PackType (..),
  create,
  scope,
  upscope,
  var,
  timeChange,
  valueChange,
  close,
  hasComment,
) where

import Data.Word (Word32, Word64)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, nullPtr)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString

data Context

-- | An open FST file.
newtype Writer = Writer (Ptr Context)

-- | A var of a 'Writer'.
type Handle = Word32

-- | How the value-change blocks are compressed.
data PackType = Zlib | FastLz | Lz4
  deriving (Eq, Show)

foreign import ccall safe "fstWriterCreate"
  c_create :: CString -> CInt -> IO (Ptr Context)
foreign import ccall unsafe "fstWriterSetTimescale"
  c_setTimescale :: Ptr Context -> CInt -> IO ()
foreign import ccall unsafe "fstWriterSetPackType"
  c_setPackType :: Ptr Context -> CInt -> IO ()
foreign import ccall unsafe "fstWriterSetComment"
  c_setComment :: Ptr Context -> CString -> IO ()
foreign import ccall unsafe "fstWriterSetScope"
  c_setScope :: Ptr Context -> CInt -> CString -> CString -> IO ()
foreign import ccall unsafe "fstWriterSetUpscope"
  c_setUpscope :: Ptr Context -> IO ()
foreign import ccall unsafe "fstWriterCreateVar"
  c_createVar :: Ptr Context -> CInt -> CInt -> Word32 -> CString -> Word32 -> IO Word32
foreign import ccall unsafe "fstWriterEmitTimeChange"
  c_timeChange :: Ptr Context -> Word64 -> IO ()
foreign import ccall unsafe "fstWriterEmitValueChange"
  c_valueChange :: Ptr Context -> Word32 -> CString -> IO ()
foreign import ccall safe "fstWriterClose"
  c_close :: Ptr Context -> IO ()
foreign import ccall safe "ccc_fst_has_comment"
  c_hasComment :: CString -> CString -> IO CInt

{- | Open an FST file for writing, in ps, with this comment. Also creates
@\<path\>.hier@, which holds the declarations until 'close' moves them into
the file.
-}
create :: FilePath -> PackType -> String -> IO Writer
create path pack comment = do
  ctx <- withCString path (\p -> c_create p 1)
  if ctx == nullPtr
    then ioError (userError ("libfst could not create " <> path))
    else do
      c_setTimescale ctx (-12)
      c_setPackType ctx $ case pack of
        Zlib -> 0
        FastLz -> 1
        Lz4 -> 2
      withCString comment (c_setComment ctx)
      pure (Writer ctx)

-- | Open a scope inside the current one.
scope :: Writer -> String -> IO ()
scope (Writer ctx) s = withCString s (\p -> withCString "" (c_setScope ctx 0 p))

-- | Close the current scope.
upscope :: Writer -> IO ()
upscope (Writer ctx) = c_setUpscope ctx

{- | Declare a var of this width in the current scope. With an alias, the
var shares that var's values.
-}
var :: Writer -> String -> Int -> Maybe Handle -> IO Handle
var (Writer ctx) leaf width alias = withCString leaf $ \p ->
  -- FST_VT_VCD_WIRE, FST_VD_IMPLICIT
  c_createVar ctx 16 0 (fromIntegral width) p (maybe 0 id alias)

-- | The values that follow are at this time (ps).
timeChange :: Writer -> Int -> IO ()
timeChange (Writer ctx) t = c_timeChange ctx (fromIntegral t)

-- | A var's new value: one character per bit, most significant first.
valueChange :: Writer -> Handle -> ByteString.ByteString -> IO ()
valueChange (Writer ctx) h bs = ByteString.unsafeUseAsCString bs (c_valueChange ctx h)

-- | Write the declarations and the last block, and close the file.
close :: Writer -> IO ()
close (Writer ctx) = c_close ctx

{- | Does the FST file at this path have this comment? 'Nothing' when it
cannot be read as an FST file.
-}
hasComment :: FilePath -> String -> IO (Maybe Bool)
hasComment path comment =
  withCString path $ \p -> withCString comment $ \c -> do
    r <- c_hasComment p c
    pure (if r < 0 then Nothing else Just (r == 1))
