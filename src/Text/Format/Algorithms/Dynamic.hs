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

module Text.Format.Algorithms.Dynamic(
       -- * Dynamic Render
       renderDynamic,
       buildDynamic,
       putDynamic
       ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.List(minimumBy)
import Prelude hiding (maximum)
import System.IO
import Text.Format.Dims
import Text.Format.Doc(Doc(..))
import Text.Format.Graphics
import Text.Format.Render

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8

--import Debug.Trace

debug :: String -> a -> a
--debug = trace
debug _ = id

newtype Frontier = Frontier { frontierRenders :: [Render] }

-- Add a 'Render' into a result set, ensuring that any subsumed
-- renders are dropped.
--
-- TODO: The asymptotic runtime of this could likely be improved by some
-- kind of tree structure; however, the design of this structure is
-- nontrivial, due to the 3+-dimensional nature of subsumption, and
-- the wierd interactions between the various kinds of column offsets.
insertRender :: Frontier -> Render -> Frontier
insertRender Frontier { frontierRenders = renders }
             ins @ Render { renderDims = dims }
    -- If the inserted element is subsumed by anything in the
    -- list, then don't insert it at all.
  | any (`subsumes` dims) (map renderDims renders) =
    Frontier { frontierRenders = renders }
    -- Otherwise, add the element to the list, and drop everything it subsumes.
  | otherwise =
    Frontier { frontierRenders = ins : filter (not . subsumes dims . renderDims)
                                              renders }

instance Monoid Frontier where
  mempty = Frontier { frontierRenders = [] }

  mappend front1 @ Frontier { frontierRenders = renders1 }
          front2 @ Frontier { frontierRenders = renders2 } =
    if length renders1 < length renders2
      then foldl insertRender front2 renders1
      else foldl insertRender front1 renders2

instance Show Frontier where
  show Frontier { frontierRenders = renders } =
    concat ("Frontier [\n" : map ((++ "\n") . show) renders ++ ["]"])

-- | Generate n spaces
makespaces :: Int -> Builder
makespaces n = fromLazyByteString (Lazy.Char8.replicate (fromIntegral n) ' ')

-- | Produce a 'Builder' that renders the 'Doc' using the optimal
-- layout engine.

-- Basic algorithm overview: each Doc is rendering into a Result,
-- which has an upper-bound (the last column at which we can start
-- without causing an overrun), an ending column (which may be a
-- relative or fixed position), and the actual render, which has a
-- "badness".  A negative upper-bound value indicates overrun.
--
-- We keep only ONE result for each upper-bound, start column pair
-- (ie. the best one).  This forms the "frontier" for the dynamic
-- programming algorithm.
--
-- Note that due to the complexity of the combinators, we CAN see
-- cubic time/space complexity.  However, actual running times are
-- much lower, especially for Docs that contain many hard linebreaks
-- and few Options.
buildDynamic :: Int
             -- ^ The maximum number of columns.
             -> Bool
             -- ^ Whether or not to render with ANSI terminal options.
             -> Doc
             -- ^ The document to render.
             -> Builder
buildDynamic maxcol ansiterm doc =
  let
    build :: Doc -> Frontier
    build d @ Char { charContent = chr } =
      -- Single characters have a single possibility, a relative
      -- ending position one beyond the start, and an upper-bound
      -- one shorter than the maximum width.
      let
        builder = const $! const $! const $! fromChar chr
        dims = Dims { dimsLines = 0, dimsStartWidth = 1,
                      dimsWidth = Relative 1, dimsNesting = 0,
                      dimsEndWidth = Relative 1, dimsMin = 0,
                      dimsBegin = Indent, dimsEnding = Normal }

        out = Frontier { frontierRenders = [Render { renderBuilder = builder,
                                                     renderDims = dims }] }
      in
        debug ("build " ++ show d ++ " =\n" ++ show out) out
    build d @ Content { contentString = txt, contentLength = len } =
      -- Text has a single possibility and a relative ending position
      -- equal to its length
      let
        builder = const $! const $! const $! fromLazyByteString txt
        dims = Dims { dimsEnding = Normal, dimsStartWidth = len,
                      dimsNesting = 0, dimsWidth = Relative len,
                      dimsBegin = Indent, dimsLines = 0, dimsMin = 0,
                      dimsEndWidth = Relative len }

        out = Frontier { frontierRenders = [Render { renderBuilder = builder,
                                                     renderDims = dims }] }
      in
        debug ("build " ++ show d ++ " =\n" ++ show out) out
    build d @ Line {} =
      -- A newline starts at the nesting level, has no overrun, and an
      -- upper-bound equal to the maximum column width.
      --
      -- Note: the upper bound is adjusted elsewhere.
      let
        builder = const $! const $! const $! fromChar '\n'
        dims = Dims { dimsLines = 1, dimsNesting = 0,
                      dimsEnding = Newline, dimsBegin = None,
                      dimsStartWidth = 0, dimsMin = 0,
                      dimsWidth = Fixed 0, dimsEndWidth = Fixed 0 }

        out = Frontier { frontierRenders = [Render { renderBuilder = builder,
                                                     renderDims = dims }] }
      in
        debug ("build " ++ show d ++ " =\n" ++ show out) out
    -- This is for an empty cat, ie. the empty document
    build Cat { catDocs = [] } = Frontier { frontierRenders = [mempty] }
    build d @ Cat { catDocs = first : rest } =
      let
        -- Glue two Results together.  This gets used in a fold.
        appendResults :: Frontier -> Doc -> Frontier
        -- The accumulated result is a Single.
        appendResults Frontier { frontierRenders = opts1 } doc2 =
          let
            Frontier { frontierRenders = opts2 } = build doc2

            -- Outer fold, over each option in the accumulated result,
            -- gluing on the next render.
            outerfold :: Frontier -> Render -> Frontier
            outerfold accum render1 =
              let
                -- Innermost fold, glues two options together
                innerfold :: Frontier -> Render -> Frontier
                innerfold accum' = insertRender accum' . mappend render1
              in
                -- Don't bother calling packResult here, we'll do
                -- it at the end anyway.
                foldl innerfold accum opts2
          in
            -- Fold it up, then use packResult.
            foldl outerfold mempty opts1

        -- Build the first item
        firstres = build first

        out = foldl appendResults firstres rest
      in
        -- Fold them all together with appendResults
        debug ("build " ++ show d ++ " =\n" ++ show out) out
    build d @ Nest { nestDelay = delay, nestDoc = inner,
                     nestAlign = alignnest, nestLevel = lvl } =
      let
        nestWidth :: Width -> Width
        -- For a fixed width, we add the nesting to the fixed width,
        -- converting to a relative width if we're aligning.
        nestWidth Fixed { fixedOffset = n } =
          if alignnest
            -- If we're aligning, convert the width to relative.
            then Relative { relOffset = n + lvl }
            -- Otherwise just add on to the fixed width.
            else Fixed { fixedOffset = n + lvl }
        -- Leave relative widths alone; they'll get incremented when
        -- we add on the first line.
        nestWidth r @ Relative {} = r
        -- For maximum widths, apply the same transform as for fixed
        -- to the fixed portion.
        nestWidth Maximum { maxFixed = n, maxRelative = rel } =
          if alignnest
            -- If we're aligning, convert the width to relative, take
            -- the maximum of the two widths.
            then Relative { relOffset = max (n + lvl) rel }
            -- Otherwise just add on to the fixed width.
            else maximum (n + lvl) (rel + lvl)

        updateRender :: Render -> Render
        updateRender r @ Render {
                           renderBuilder = builder,
                           renderDims = dims @ Dims { dimsStartWidth = swidth,
                                                      dimsMin = minwidth,
                                                      dimsWidth = width,
                                                      dimsNesting = nesting,
                                                      dimsEndWidth = ewidth,
                                                      dimsBegin = begin }
                         } =
          let
            -- We add indentation if the rendering definitely needs it
            -- AND we're not delaying.  If we're delaying, then the
            -- indentation will be added at the next newline.
            addspaces = begin == Indent && not delay
            -- Wrap up the render functions in code that alters the
            -- nesting and column numbers.
            (newbuilder, newswidth, newnesting, newmin) =
              case (alignnest, addspaces) of
              -- We're aligning, and we need to add spaces.  Set the
              -- column and the nesting to the old column plus the
              -- offset.
              (True, True) ->
                let
                  nspaces = max 0 lvl
                in
                  (\sgr _ c -> makespaces nspaces `mappend`
                               builder sgr (c + nspaces) (c + nspaces),
                   swidth + nspaces, 0, minwidth - nspaces)
              -- We're aligning, but don't need spaces.  Leave the
              -- column as is, and don't generate any spaces.
              (True, False) -> (\sgr _ c -> builder sgr (c + lvl) c,
                                swidth, 0, minwidth)
              -- We're incrementing nesting and generating spaces.
              -- Generate a number of spaces equal to the max of zero
              -- or the difference.  Set the column to the greater of
              -- the current column or the nesting.
              (False, True) ->
                (\sgr n c ->
                  if c < lvl
                    then makespaces (lvl - c) `mappend`
                         builder sgr (n + lvl) lvl
                    else builder sgr (n + lvl) c,
                 swidth, nesting, lvl + swidth)
              -- We're incrementing nesting, but not generating
              -- spaces.  Leave the column as-is.
              (False, False) -> (\sgr n c -> builder sgr (n + lvl) c,
                                 swidth, nesting + lvl, minwidth)
          in
            r { renderBuilder = newbuilder,
                renderDims = dims { dimsNesting = newnesting,
                                    dimsStartWidth = newswidth,
                                    dimsWidth = nestWidth width,
                                    dimsEndWidth = nestWidth ewidth,
                                    dimsMin = newmin } }

        Frontier { frontierRenders = opts } = build inner
        out = Frontier { frontierRenders = map updateRender opts }
      in
        debug ("build " ++ show d ++ " =\n" ++ show out) out
    build d @ Choose { chooseOptions = options } =
      let
        out = mconcat (map build options)
      in
        debug ("build " ++ show d ++ " =\n" ++ show out) out
    build Graphics { graphicsSGR = sgr2, graphicsDoc = inner }
      -- Only do graphics if the ansiterm flag is set.
      | ansiterm =
        let
          -- Insert graphics control characters without updating
          -- column numbers, as they aren't visible.
          wrapBuilder r @ Render { renderBuilder = builder } =
            r { renderBuilder = \sgr1 n c -> switchGraphics sgr1 sgr2 `mappend`
                                             builder sgr2 n c `mappend`
                                             switchGraphics sgr2 sgr1 }

          Frontier { frontierRenders = opts } = build inner

          wrapped = map wrapBuilder opts
        in
          Frontier { frontierRenders = wrapped }
      -- Otherwise, skip it entirely
      | otherwise = build inner

    -- Pick the best result out of a set of results.  Used at the end to
    -- pick the final result.
    bestRenderInOpts :: [Render] -> Render
    bestRenderInOpts =
      let
        -- | Calculate the overrun of a width.
        overrun :: Width -> Int
        overrun Fixed { fixedOffset = n } = max 0 (n - maxcol)
        overrun Relative { relOffset = n } = max 0 (n - maxcol)
        overrun Maximum { maxFixed = fixed, maxRelative = rel } =
          max 0 (max fixed rel - maxcol)

        -- | Compare two Renders.  Less than means better.
        compareRenders Render { renderDims = Dims { dimsLines = lines1,
                                                    dimsWidth = width1 } }
                       Render { renderDims = Dims { dimsLines = lines2,
                                                    dimsWidth = width2 } } =
          case compare (overrun width1) (overrun width2) of
            EQ -> compare lines1 lines2
            out -> out
      in
        minimumBy compareRenders

    -- Call build, get the result, then pick the best one.
    Render { renderBuilder = result } =
      let
        Frontier { frontierRenders = opts } = build doc
      in
        bestRenderInOpts opts
  in
    result Default 0 0

-- | Render a 'Doc' as a lazy bytestring using an optimal layout
-- rendering engine.  The engine will render the document in the
-- fewest number of lines possible without exceeding the maximum
-- column width.
renderDynamic :: Int
              -- ^ The maximum number of columns.
              -> Bool
              -- ^ Whether or not to render with ANSI terminal options.
              -> Doc
              -- ^ The document to render.
              -> Lazy.ByteString
renderDynamic cols color = toLazyByteString . buildDynamic cols color

-- | Output the entire 'Doc', as rendered by 'renderDynamic' to the
-- given 'Handle'.
putDynamic :: Handle
           -- ^ The 'Handle' to which to write output
           -> Int
           -- ^ The maximum number of columns.
           -> Bool
           -- ^ Whether or not to render with ANSI terminal options.
           -> Doc
           -- ^ The document to render.
           -> IO ()
putDynamic handle cols color =
  toByteStringIO (Strict.hPut handle) . buildDynamic cols color
