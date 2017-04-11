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
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
             MultiParamTypeClasses, UndecidableInstances #-}

-- | A pretty printer implementation, based loosely on the
-- Wadler-Leijin pretty printer, but redesigned to facilitate a
-- dynamic programming optimal layout algorithm.
--
-- This pretty printer module trades some of the generality of the
-- Wadler-Leijin scheme in order to facilitate an efficient optimizing
-- layout engine.  The nesting, column, and width combinators are
-- removed.
module Text.Format(
       module Text.Format.Class,
       module Text.Format.Doc,
       module Text.Format.Algorithms.Fast,
       module Text.Format.Algorithms.OneLine,

       -- * Rendering @Doc@s
       -- ** Single-Line Render

       -- ** Fast Render

       -- ** Optimal Render
       renderOptimal,
       buildOptimal,
       putOptimal
       ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.List(minimumBy)
import Data.Monoid hiding ((<>))
import Prelude hiding ((<$>), concat, maximum, minimum)
import System.IO
import Text.Format.Algorithms.Fast
import Text.Format.Algorithms.OneLine
import Text.Format.Class
import Text.Format.Dims
import Text.Format.Doc
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

-- | A result.  This is split into 'Single' and 'Multi' in order to
-- optimize for the common case of a single possible rendering.
-- Otherwise, it would be perfectly fine to represent everything as a
-- HashMap.
data Result =
    -- | A single possible rendering.
    Single {
      -- | The rendered document.
      singleRender :: !Render
    }
    -- | Multiple possible renderings.
  | Multi {
      -- | A multi-level map.  The first map is indexed by the column
      -- upper-bound (meaning the first column at which using any of
      -- the contents will cause an overrun).  The second map is
      -- indexed by the ending column.
      multiOptions :: !Frontier
    }

instance Monoid Result where
  mempty = Single { singleRender = mempty }

  -- If both are single, just concatenate them.
  mappend Single { singleRender = render1 } Single { singleRender = render2 } =
    Single { singleRender = render1 `mappend` render2 }
  -- If the first is single and the second is multi, we have to fold
  -- over all the options and glue on the first render.
  mappend Single { singleRender = render1 }
          Multi { multiOptions = Frontier { frontierRenders = opts } } =
    let
      -- Glue the first render on to an option.
      foldfun :: Frontier -> Render -> Frontier
      foldfun accum = insertRender accum . mappend render1
    in
      -- Fold it up, then use packResult.
      packResult (foldl foldfun mempty opts)
  -- If the first render is a multi, then we'll need to
  -- glue the second render on to each option.
  mappend Multi { multiOptions = Frontier { frontierRenders = opts } }
          Single { singleRender = render2 } =
    let
      -- Glue the first render on to an option.
      foldfun :: Frontier -> Render -> Frontier
      foldfun accum render1 = insertRender accum (render1 `mappend` render2)
    in
      -- Fold it up, then use packResult.
      packResult (foldl foldfun mempty opts)
  -- If both are multis, then we need a two-level fold
  mappend Multi { multiOptions = Frontier { frontierRenders = opts1 } }
          Multi { multiOptions = Frontier { frontierRenders = opts2 } } =
    let
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
      packResult (foldl outerfold mempty opts1)

-- | Generate n spaces
makespaces :: Int -> Builder
makespaces n = fromLazyByteString (Lazy.Char8.replicate (fromIntegral n) ' ')

-- If a result only has one possibility, convert it to a Single.
-- Otherwise, leave it.
packResult :: Frontier -> Result
packResult Frontier { frontierRenders = [opt] } = Single { singleRender = opt }
packResult Frontier { frontierRenders = opts }  =
  Multi { multiOptions = Frontier { frontierRenders = opts } }

-- | Combine two results into a Multi.  This achieves the same result
-- as HashMap.unionWith bestRender (meaning, union these maps,
-- combining using bestRender to pick when both maps have a given
-- index), but handles Singles as well.
mergeResults :: Result -> Result -> Result
-- Single is equivalent to a single-entry HashMap
mergeResults s1 @ Single { singleRender = r1 @ Render { renderDims = d1 } }
             s2 @ Single { singleRender = r2 @ Render { renderDims = d2 } }
  | subsumes d1 d2 = s1
  | subsumes d2 d1 = s2
  | otherwise = Multi { multiOptions = Frontier { frontierRenders = [r1, r2] } }
mergeResults Single { singleRender = render }
             Multi { multiOptions = opts } =
  packResult (insertRender opts render)
-- This operation is commutative
mergeResults m @ Multi {} s @ Single {} = mergeResults s m
-- Otherwise it's a straightaway HashMap union
mergeResults Multi { multiOptions = opts1 } Multi { multiOptions = opts2 } =
  packResult (opts1 `mappend` opts2)

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
buildOptimal :: Int
             -- ^ The maximum number of columns.
             -> Bool
             -- ^ Whether or not to render with ANSI terminal options.
             -> Doc
             -- ^ The document to render.
             -> Builder
buildOptimal maxcol ansiterm doc =
  let
    build :: Doc -> Result
    build d
      | debug ("build\n  " ++ show d ++ " =") False = undefined
    -- For char, bytestring, and lazy bytestring,
    build Char { charContent = chr } =
      let
        builder = const $! const $! const $! fromChar chr
        dims = Dims { dimsLines = 0, dimsStartWidth = 1,
                      dimsWidth = Relative 1, dimsNesting = 0,
                      dimsEndWidth = Relative 1, dimsMin = 0,
                      dimsBegin = Indent, dimsEnding = Normal }
      in
        -- Single characters have a single possibility, a relative
        -- ending position one beyond the start, and an upper-bound
        -- one shorter than the maximum width.
        Single { singleRender = Render { renderBuilder = builder,
                                         renderDims = dims } }
    build Content { contentString = txt, contentLength = len } =
      let
        builder = const $! const $! const $! fromLazyByteString txt
        dims = Dims { dimsEnding = Normal, dimsStartWidth = len,
                      dimsNesting = 0, dimsWidth = Relative len,
                      dimsBegin = Indent, dimsLines = 0, dimsMin = 0,
                      dimsEndWidth = Relative len }
      in
        -- Text has a single possibility and a relative ending position
        -- equal to its length
       Single { singleRender = Render { renderBuilder = builder,
                                        renderDims = dims } }
    build Line {} =
      let
        builder = const $! const $! const $! fromChar '\n'
        dims = Dims { dimsLines = 1, dimsNesting = 0,
                      dimsEnding = Newline, dimsBegin = None,
                      dimsStartWidth = 0, dimsMin = 0,
                      dimsWidth = Fixed 0, dimsEndWidth = Fixed 0 }
      in
        -- A newline starts at the nesting level, has no overrun, and an
        -- upper-bound equal to the maximum column width.
        --
        -- Note: the upper bound is adjusted elsewhere.
        Single { singleRender = Render { renderBuilder = builder,
                                         renderDims = dims } }
    -- This is for an empty cat, ie. the empty document
    build Cat { catDocs = [] } = mempty
    build Cat { catDocs = first : rest } =
      let
        -- Glue two Results together.  This gets used in a fold.
        appendResults :: Result -> Doc -> Result
        -- The accumulated result is a Single.
        appendResults res1 = mappend res1 . build

        -- Build the first item
        firstres = build first
      in
        -- Fold them all together with appendResults
        foldl appendResults firstres rest
    build Nest { nestDelay = delay, nestDoc = inner,
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
                           renderDims = d @ Dims { dimsStartWidth = swidth,
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
                renderDims = d { dimsNesting = newnesting,
                                 dimsStartWidth = newswidth,
                                 dimsWidth = nestWidth width,
                                 dimsEndWidth = nestWidth ewidth,
                                 dimsMin = newmin } }

        res = build inner
      in case res of
        -- Update the render for a Single.
        s @ Single { singleRender = r } -> s { singleRender = updateRender r }
        -- Update all renders for a Multi.
        m @ Multi { multiOptions = Frontier { frontierRenders = opts } } ->
          let
            updated = map updateRender opts
          in
            m { multiOptions = Frontier { frontierRenders = updated } }
    build Choose { chooseOptions = options } =
      let
        -- Build up all the components
        results = map build options
      in
        -- Now merge them into an minimal set of options
        foldl1 mergeResults results
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
        in case build inner of
          s @ Single { singleRender = render } ->
            s { singleRender = wrapBuilder render }
          m @ Multi { multiOptions = Frontier { frontierRenders = opts } } ->
            let
              wrapped = map wrapBuilder opts
            in
              m { multiOptions = Frontier { frontierRenders = wrapped } }
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
      case build doc of
        Single { singleRender = render } -> render
        Multi { multiOptions = Frontier { frontierRenders = opts } } ->
          bestRenderInOpts opts
  in
    result Default 0 0

-- | Render a 'Doc' as a lazy bytestring using an optimal layout
-- rendering engine.  The engine will render the document in the
-- fewest number of lines possible without exceeding the maximum
-- column width.
renderOptimal :: Int
              -- ^ The maximum number of columns.
              -> Bool
              -- ^ Whether or not to render with ANSI terminal options.
              -> Doc
              -- ^ The document to render.
              -> Lazy.ByteString
renderOptimal cols color = toLazyByteString . buildOptimal cols color

-- | Output the entire 'Doc', as rendered by 'renderOptimal' to the
-- given 'Handle'.
putOptimal :: Handle
           -- ^ The 'Handle' to which to write output
           -> Int
           -- ^ The maximum number of columns.
           -> Bool
           -- ^ Whether or not to render with ANSI terminal options.
           -> Doc
           -- ^ The document to render.
           -> IO ()
putOptimal handle cols color =
  toByteStringIO (Strict.hPut handle) . buildOptimal cols color
