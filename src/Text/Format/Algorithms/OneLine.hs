-- Copyright (c) 2017 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

-- | Single-line rendering.  This algorithm always renders to a single
-- line (or else it indicates failure).
module Text.Format.Algorithms.OneLine(
       renderOneLine,
       buildOneLine,
       putOneLine,
       ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Text.Format.Doc
import System.IO

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

-- | Produce a 'Builder' that renders the 'Doc' to one line.
buildOneLine :: Doc -> Maybe Builder
buildOneLine doc =
  let
     build Char { charContent = chr } = fromChar chr
     build Content { contentString = builder } = fromLazyByteString builder
     build Line { lineKind = Break } = fromChar ' '
     build Line { lineKind = Soft } = mempty
     build Line { lineKind = Hard } = error "Should not see a hard line"
     build Cat { catDocs = docs } = mconcat (map build docs)
     build Nest { nestDoc = inner } = build inner
     build Choose { chooseOptions = opts } = build (head opts)
     build Graphics { graphicsDoc = inner } = build inner
  in do
    flat <- flatten doc
    return $! build flat

-- | Render the entire 'Doc' to one line.  Good for output that
-- will be read only by a machine, where newlines are not important at all
renderOneLine :: Doc -> Maybe Lazy.ByteString
renderOneLine doc =
  do
    builder <- buildOneLine doc
    return $! toLazyByteString builder

-- | Output the entire 'Doc', as rendered by 'renderOneLine' to the
-- given 'Handle'.
putOneLine :: Handle -> Doc -> IO Bool
putOneLine handle doc =
  case buildOneLine doc of
    Just builder ->
      do
        toByteStringIO (Strict.hPut handle) builder
        return True
    Nothing -> return False
