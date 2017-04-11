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

module Text.Format.Render(
       Render(..)
       ) where

import Blaze.ByteString.Builder
import Text.Format.Doc
import Text.Format.Dims

import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8

--import Debug.Trace

debug :: String -> a -> a
--debug = trace
debug _ = id

-- | A rendering of a document.
data Render =
  Render {
    -- | A builder that constructs the document.
    renderBuilder :: !(Graphics -> Int -> Int -> Builder),
    -- | Indentation mode for the next document.
    renderDims :: !Dims
  }

instance Show Render where
  show Render { renderBuilder = builder, renderDims = dims } =
    "Render { renderDims = " ++ show dims ++
    ", renderBuilder = " ++ show (toLazyByteString (builder Default 0 0)) ++
    " }"

instance Monoid Render where
  mempty = Render { renderBuilder = const mempty, renderDims = mempty }

  mappend r1 r2
    | debug ("append\n  " ++ show r1 ++ "\n  " ++ show r2 ++ " =") False =
      undefined
  mappend r1 @ Render { renderDims = Dims { dimsEnding = Newline } }
          r2 @ Render { renderDims = d2 @ Dims { dimsBegin = Indent,
                                                 dimsNesting = nest2,
                                                 dimsMin = min2 } }
    | nest2 > 0 =
      let
        builder = const $! const $! const $! makespaces nest2
        newmin = max 0 (min2 - nest2)
        spaces = Render { renderDims = Dims { dimsEnding = Normal,
                                              dimsStartWidth = nest2,
                                              dimsBegin = Indent,
                                              dimsWidth = Relative nest2,
                                              dimsLines = 0, dimsMin = 0,
                                              dimsNesting = 0,
                                              dimsEndWidth = Relative nest2 },
                          renderBuilder = builder }
    in
      r1 `mappend` spaces `mappend`
      r2 { renderDims = d2 { dimsNesting = 0, dimsMin = newmin } }
  mappend Render { renderDims = dims1 @ Dims { dimsEndWidth = ewidth1,
                                               dimsEnding = end1 },
                   renderBuilder = build1 }
          Render { renderDims = dims2 @ Dims { dimsBegin = begin2 },
                   renderBuilder = build2 } =
    let
      newbuild = case (end1, begin2, ewidth1) of
        -- We end with a newline, and begin with content.  Do
        -- indentation.  Note that indentation for any nesting in
        -- builder1 and builder2 will be handled internally.
        (Newline, Indent, _) -> \sgr n c -> build1 sgr n c `mappend`
                                            makespaces n `mappend`
                                            build2 sgr n n
        -- We end with something other than a newline, meaning we need
        -- to calculate the column for the second builder.
        --
        -- For a fixed width, we set the column to the offset
        (_, _, Fixed { fixedOffset = off }) ->
          \sgr n c -> build1 sgr n c `mappend` build2 sgr n (n + off)
        -- For a relative offset, we add the offset to the previous column.
        (_, _, Relative { relOffset = off }) ->
          \sgr n c -> build1 sgr n c `mappend` build2 sgr n (c + off)
        -- For a maximum offset, we take the max.
        (_, _, Maximum { maxFixed = fixed, maxRelative = rel }) ->
          \sgr n c -> build1 sgr n c `mappend`
                      build2 sgr n (max fixed (c + rel))

      newdims = dims1 `mappend` dims2

      out = Render { renderBuilder = newbuild, renderDims = newdims }
    in
      debug ("    " ++ show out ++ "\n\n") out

-- | Generate n spaces
makespaces :: Int -> Builder
makespaces n = fromLazyByteString (Lazy.Char8.replicate (fromIntegral n) ' ')
