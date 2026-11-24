{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

How this package prints a message: as one line on stderr, written whole.

stderr is unbuffered, and 'System.IO.hPutStrLn' writes an unbuffered
'System.IO.Handle' a character at a time, taking the 'System.IO.Handle' for
each one. So the messages of threads that print at the same time (tests
that fail together in a parallel run) were interleaved character by
character. 'say' encodes the whole line first, and gives it to the
'System.IO.Handle' as one buffer: one @write@ on an unbuffered handle.
-}
module Clash.CircuitContext.Internal.Message (say) where

import GHC.Foreign (withCStringLen)
import GHC.IO.Encoding (TextEncoding, mkTextEncoding)
import System.IO (hPutBuf, stderr)
import System.IO.Unsafe (unsafePerformIO)

{- | UTF-8, but a byte of a file name that the file system's encoding could
not decode (see "GHC.IO.Encoding") is written as that byte, as it is in the
file name.
-}
utf8 :: TextEncoding
utf8 = unsafePerformIO (mkTextEncoding "UTF-8//ROUNDTRIP")
{-# NOINLINE utf8 #-}

{- | Print this message on stderr, after @clash-circuit-context: @, as one
line, in one write.
-}
say :: String -> IO ()
say msg =
  withCStringLen utf8 ("clash-circuit-context: " <> msg <> "\n") $ \(p, n) ->
    hPutBuf stderr p n
