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

module Text.Format.Dims(
       Width(..),
       Ending(..),
       Begin(..),
       Dims(..),

       maximum,
       maxWidth,
       subsumesWidth,
       subsumes
       ) where

import Data.Hashable
import Prelude hiding (maximum)

--import Debug.Trace

debug :: String -> a -> a
--debug = trace
debug _ = id

-- | A description of the ending.
data Ending =
    -- | Ended with a newline, so do a full indent.
    Newline
    -- | Ended with normal content, so no indent.
  | Normal
    -- | Take the previous document's ending.
  | Prev
    deriving (Eq, Show)

instance Monoid Ending where
  mempty = Prev

  mappend end Prev = end
  mappend _ end = end

-- | A description of the indentation required.
data Begin =
    -- | begins with content, so indentation is needed.
    Indent
    -- | Begins with a newline, so do not indent.
  | None
    -- | Take the next documents beginning.
  | Next
    deriving (Eq, Show)

-- | Width data type.  Represents the width of a document.
--
-- Widths can be fixed, relative, or the maximum of the two.  Fixed
-- means "this width exactly".  Relative widths are increased every
-- time something is prepended to the first line of a document.
-- Maximum means "relative, but no less than a fixed amount".
data Width =
    -- | An absolute column offset.
    Fixed { fixedOffset :: !Int }
    -- | A relative column offset.
  | Relative { relOffset :: !Int }
    -- | The greater of a relative column offset and an absolute
    -- column offset.
  | Maximum {
      -- | This many columns offset from a relative point.
      maxRelative :: !Int,
      -- | But not less than this value.
      maxFixed :: !Int
    }
    deriving Show

data Dims =
  Dims {
    -- | Starting line fragment width.
    dimsStartWidth :: !Int,
    -- | Number of extra spaces to generate.
    dimsNesting :: !Int,
    -- | Minimum size of the first line.
    dimsMin :: !Int,
    -- | Width: Number of columns at the widest point in the complete
    -- lines.
    dimsWidth :: !Width,
    -- | Ending line fragment width.
    dimsEndWidth :: !Width,
    -- | The number of lines in the document.
    dimsLines :: !Word,
    -- | Indentation mode for the next document.
    dimsEnding :: !Ending,
    -- | Whether or not the builder needs indentation inserted before.
    dimsBegin :: !Begin
  }
  deriving (Eq)

instance Show Dims where
  show Dims { dimsWidth = mwidth, dimsStartWidth = swidth,
              dimsNesting = nesting, dimsMin = minwidth,
              dimsEndWidth = ewidth, dimsLines = lns,
              dimsEnding = end, dimsBegin = begin } =
    "Dims { dimsStartWidth = " ++ show swidth ++
    ", dimsNesting = " ++ show nesting ++
    ", dimsMin = " ++ show minwidth ++
    ", dimsWidth = " ++ show mwidth ++
    ", dimsEndWidth = " ++ show ewidth ++
    ", dimsLines = " ++ show lns ++
    ", dimsEnding = " ++ show end ++
    ", dimsBegin = " ++ show begin ++
    " }"

instance Eq Width where
  c1 == c2 = compare c1 c2 == EQ

instance Hashable Width where
  hashWithSalt s Fixed { fixedOffset = n } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` n
  hashWithSalt s Relative { relOffset = n } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` n
  hashWithSalt s Maximum { maxFixed = fixed, maxRelative = rel } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` fixed `hashWithSalt` rel

instance Ord Width where
  compare Fixed { fixedOffset = n1 } Fixed { fixedOffset = n2 } = compare n1 n2
  compare Fixed { fixedOffset = n }
          Maximum { maxFixed = fixed, maxRelative = rel } =
    case compare n fixed of
      EQ -> case compare n rel of
        EQ -> LT
        out -> out
      out -> out
  compare Fixed { fixedOffset = n1 } Relative { relOffset = n2 } =
    case compare n1 n2 of
      EQ -> LT
      out -> out
  compare m @ Maximum {} f @ Fixed {} =
    case compare f m of
      LT -> GT
      EQ -> EQ
      GT -> LT
  compare Maximum { maxFixed = fixed1, maxRelative = rel1 }
          Maximum { maxFixed = fixed2, maxRelative = rel2 } =
    case compare fixed1 fixed2 of
      EQ -> compare rel1 rel2
      out -> out
  compare Maximum { maxFixed = fixed, maxRelative = rel }
          Relative { relOffset = n } =
    case compare rel n of
      EQ -> case compare fixed n of
        EQ -> GT
        out -> out
      out -> out
  compare r @ Relative {} f @ Fixed {} =
    case compare f r of
      LT -> GT
      EQ -> EQ
      GT -> LT
  compare r @ Relative {} m @ Maximum {} =
    case compare m r of
      LT -> GT
      EQ -> EQ
      GT -> LT
  compare Relative { relOffset = n1 } Relative { relOffset = n2 } =
    compare n1 n2

instance Monoid Begin where
  mempty = Next

  mappend Next begin = begin
  mappend begin _ = begin

instance Monoid Width where
  mempty = Relative { relOffset = 0 }

  -- If the second ending width is fixed, then it doesn't change
  mappend _ f @ Fixed {} = f
  -- If the first is fixed and the second is relative, then
  -- advance the first by the relative offset, unless the first
  -- document ends with a line.
  mappend Fixed { fixedOffset = start } Relative { relOffset = n } =
    Fixed { fixedOffset = start + n }
  -- If the first is fixed and the second is a maximum, then we can
  -- figure out which is the larger.
  mappend Fixed { fixedOffset = start } Maximum { maxFixed = fixed,
                                                       maxRelative = rel } =
    Fixed { fixedOffset = max fixed (start + rel) }
  -- If both are relative, just add them and make a new relative.
  mappend Relative { relOffset = start } Relative { relOffset = n } =
    Relative { relOffset = start + n }
  -- If we combine a relative and a maximum, then add the relative
  -- offset to the relative portion of the maximum
  mappend Relative { relOffset = start } Maximum { maxFixed = fixed,
                                                   maxRelative = n } =
    maximum fixed (start + n)
  mappend Maximum { maxFixed = fixed, maxRelative = rel }
          Relative { relOffset = n } =
    maximum fixed (rel + n)
  -- If both are a maximum, then the resulting relative portion is the
  -- sum of the two relative portions.  The resulting fixed portion is
  -- the greater of the second fixed portion, or the first fixed portion
  -- plus the second relative portion.
  mappend Maximum { maxFixed = fixed1, maxRelative = rel1 }
          Maximum { maxFixed = fixed2, maxRelative = rel2 } =
    maximum (max fixed2 (fixed1 + rel2)) (rel1 + rel2)

instance Monoid Dims where
  mempty = Dims { dimsStartWidth = 0, dimsWidth = Relative 0,
                  dimsEndWidth = Relative 0, dimsEnding = mempty,
                  dimsNesting = 0, dimsLines = 0,
                  dimsBegin = mempty, dimsMin = 0 }

  mappend r1 r2
    | debug ("append\n  " ++ show r1 ++ "\n  " ++ show r2 ++ " =") False =
      undefined
  mappend r1 @ Dims { dimsEnding = Newline }
          r2 @ Dims { dimsBegin = Indent, dimsNesting = nest2,
                      dimsMin = min2 }
    | nest2 > 0 =
      let
        newmin = max 0 (min2 - nest2)
        spaces = Dims { dimsEnding = Normal, dimsStartWidth = nest2,
                        dimsBegin = Indent, dimsWidth = Relative nest2,
                        dimsLines = 0, dimsNesting = 0, dimsMin = 0,
                        dimsEndWidth = Relative nest2 }
    in
      r1 `mappend` spaces `mappend` r2 { dimsNesting = 0, dimsMin = newmin }
  mappend Dims { dimsEnding = end1, dimsBegin = begin1, dimsNesting = nest1,
                 dimsLines = lines1, dimsStartWidth = swidth1,
                 dimsWidth = width1, dimsEndWidth = ewidth1,
                 dimsMin = min1 }
          Dims { dimsEnding = end2, dimsBegin = begin2, dimsMin = min2,
                 dimsLines = lines2, dimsStartWidth = swidth2,
                 dimsWidth = width2, dimsEndWidth = ewidth2 } =
    let
      -- The new begin and end states are from the monoid operations.
      newend = end1 `mappend` end2
      newbegin = begin1 `mappend` begin2

      -- Calculate the width of the middle line by adding the second
      -- start width onto the first ending width.
      midline = case (ewidth1, min2) of
        (Fixed { fixedOffset = off }, 0) ->
          Fixed { fixedOffset = off + swidth2 }
        (Fixed {}, mwidth) -> Fixed { fixedOffset = mwidth }
        (Relative { relOffset = off }, 0) ->
          Relative { relOffset = off + swidth2 }
        (Relative { relOffset = off }, mwidth) ->
          maximum mwidth (off + swidth2)
        (Maximum { maxFixed = fixed, maxRelative = rel }, mwidth) ->
          maximum (max mwidth (fixed + swidth2)) (rel + swidth2)

      (newswidth, newmin) =
        if lines1 == 0
          then (swidth1 + swidth2, max min1 min2)
          else (swidth1, min1)

      newewidth = mappend ewidth1 ewidth2
      newwidth2 = mappend ewidth1 width2

      -- The new width is the maximum of the two max width, the second
      -- end width, and the width of the newly-formed middle line.
      newwidth = maxWidth (maxWidth newewidth midline)
                          (maxWidth width1 newwidth2)

      out = Dims { dimsEnding = newend, dimsBegin = newbegin,
                   dimsLines = lines1 + lines2, dimsStartWidth = newswidth,
                   dimsWidth = newwidth, dimsEndWidth = newewidth,
                   dimsNesting = nest1, dimsMin = newmin }
    in
      debug ("    " ++ show out ++ "\n\n") out

maximum :: Int -> Int -> Width
maximum fixed rel
  | rel >= fixed = Relative { relOffset = rel }
  | otherwise = Maximum { maxFixed = fixed, maxRelative = rel }

maxWidth :: Width -> Width -> Width
maxWidth Fixed { fixedOffset = n1 } Fixed { fixedOffset = n2 } =
  Fixed { fixedOffset = max n1 n2 }
maxWidth Fixed { fixedOffset = fixed } Relative { relOffset = rel } =
  maximum fixed rel
maxWidth r @ Relative {} f @ Fixed {} = maxWidth f r
maxWidth Fixed { fixedOffset = fixed1 }
         Maximum { maxFixed = fixed2, maxRelative = rel } =
  maximum (max fixed1 fixed2) rel
maxWidth m @ Maximum {} f @ Fixed {} = maxWidth f m
maxWidth Relative { relOffset = rel1 } Relative { relOffset = rel2 } =
  Relative { relOffset = max rel1 rel2 }
maxWidth Relative { relOffset = rel1 }
         Maximum { maxFixed = fixed, maxRelative = rel2 } =
  maximum fixed (max rel1 rel2)
maxWidth m @ Maximum {} r @ Relative {} = maxWidth r m
maxWidth Maximum { maxFixed = fix1, maxRelative = rel1 }
         Maximum { maxFixed = fix2, maxRelative = rel2 } =
  maximum (max fix1 fix2) (max rel1 rel2)

-- | The first width cannot possibly be greater than the second.
subsumesWidth :: Width -> Width -> Bool
-- Simple comparisons: if the upper bound is greater, the lines are
-- less, and the column is less, then the render is always better.
subsumesWidth Fixed { fixedOffset = col1 } Fixed { fixedOffset = col2 } =
  col1 <= col2
subsumesWidth Relative { relOffset = col1 } Relative { relOffset = col2 } =
  col1 <= col2
-- A fixed column offset can be subsumed by a relative offset
subsumesWidth Fixed { fixedOffset = col1 } Relative { relOffset = col2 } =
  col1 <= col2
-- For minimum and maximum cases, we use the three above rules to
-- reason through them.
--
-- A relative can subsume a maximum by the 2nd and 3rd rules.
subsumesWidth Maximum { maxFixed = fixed1, maxRelative = rel1 }
              Relative { relOffset = col2 } =
  fixed1 <= col2 && rel1 <= col2
-- A maximum can subsume a fixed by the 1st or the 3rd rule.
subsumesWidth Fixed { fixedOffset = col1 } Maximum { maxRelative = rel2,
                                                     maxFixed = fixed2 } =
  col1 <= rel2 || col1 <= fixed2
-- A maximum can subsume a relative by the 2nd rule.
subsumesWidth Relative { relOffset = col1 } Maximum { maxRelative = rel2 } =
  col1 <= rel2
-- For two maximums, apply all three rules
subsumesWidth Maximum { maxRelative = rel1, maxFixed = fixed1 }
              Maximum { maxRelative = rel2, maxFixed = fixed2 } =
  rel1 <= rel2 && fixed1 <= fixed2 && fixed1 <= rel2
subsumesWidth _ _ = False

-- | Determine whether the first 'Render' is strictly better than the second.
subsumes :: Dims -> Dims -> Bool
-- Simple comparisons: if the width is subsumed and the lines are less
subsumes Dims { dimsWidth = width1, dimsStartWidth = swidth1,
                dimsLines = lines1, dimsEndWidth = ewidth1,
                dimsMin = min1 }
         Dims { dimsWidth = width2, dimsStartWidth = swidth2,
                dimsLines = lines2, dimsEndWidth = ewidth2,
                dimsMin = min2 } =
  lines1 <= lines2 && swidth1 <= swidth2 && min1 <= min2 &&
  subsumesWidth width1 width2 && subsumesWidth ewidth1 ewidth2
