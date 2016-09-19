-- Copyright (c) 2015 Eric McCorkle.  All rights reserved.
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
       -- * Basic Definitions

       -- ** Types
       Doc,
       Graphics(..),

       -- ** Type Classes
       Format(..),
       FormatM(..),

       -- * Creating @Doc@s

       -- ** Constructors

       -- *** Basic
       empty,
       line,
       linebreak,
       hardline,
       softline,
       softbreak,

       -- *** From datatypes
       char,
       string,
       bytestring,
       lazyBytestring,

       -- *** Literals
       lparen,
       rparen,
       lbrack,
       rbrack,
       lbrace,
       rbrace,
       langle,
       rangle,
       squote,
       dquote,
       backquote,
       comma,
       semi,
       colon,
       dot,
       bar,
       backslash,
       equals,
       space,

       -- *** Derived
       nest,
       indent,
       alignOffset,
       align,
       squoted,
       dquoted,
       parens,
       brackets,
       braces,
       angles,
       list,

       -- *** Graphics Mode
       graphics,
       dullWhite,
       dullRed,
       dullYellow,
       dullGreen,
       dullBlue,
       dullCyan,
       dullMagenta,
       dullBlack,
       vividWhite,
       vividRed,
       vividYellow,
       vividGreen,
       vividBlue,
       vividCyan,
       vividMagenta,
       vividBlack,
       dullWhiteBackground,
       dullRedBackground,
       dullYellowBackground,
       dullGreenBackground,
       dullBlueBackground,
       dullCyanBackground,
       dullMagentaBackground,
       dullBlackBackground,
       vividWhiteBackground,
       vividRedBackground,
       vividYellowBackground,
       vividGreenBackground,
       vividBlueBackground,
       vividCyanBackground,
       vividMagentaBackground,
       vividBlackBackground,
       bold,
       debold,
       underline,
       deunderline,

       -- ** Combining @Doc@s

       -- *** Basic
       beside,
       concat,
       choose,

       -- *** Derived
       (<>),
       (<+>),
       (<!>),
       (<$>),
       (<$$>),
       (</>),
       (<//>),
       hsep,
       hcat,
       vsep,
       vcat,
       sep,
       cat,
       fillSep,
       fillCat,
       enclose,
       punctuate,
       encloseSep,

       -- ** Transforming @Doc@s
       flatten,
       group,

       -- * Rendering @Doc@s

       -- ** Single-Line Render
       renderOneLine,
       buildOneLine,
       putOneLine,

       -- ** Fast Render
       renderFast,
       buildFast,
       putFast,

       -- ** Optimal Render
       renderOptimal,
       buildOptimal,
       putOptimal,

       -- ** Greedy Render
       renderGreedy,
       buildGreedy,
       putGreedy
       ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Arrow((***))
import Control.Monad
import Data.Hashable
import Data.List(intersperse, minimumBy, sort, sortBy)
import Data.Maybe
import Data.Monoid
import Data.Word
import Prelude hiding ((<$>), concat, maximum, minimum)
import System.Console.ANSI
import System.IO

import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict.UTF8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import qualified Data.ByteString.Lazy.UTF8 as Lazy.UTF8

--import Debug.Trace

debug :: String -> a -> a
--debug = trace
debug _ = id

-- | Datatype representing a formatted document.

data LineKind =
    -- | Unerasable linebreak
    Hard
    -- | Linebreak replaced with nothing
  | Soft
    -- | Linebreak replaced with a space
  | Break
    deriving (Ord, Eq, Enum, Show)

-- Docs are organized into a tree structure whose nodes dictate the
-- formatting of the generated text.  These are rendered by the
-- various rendering engines into a Builder (from the blaze-builder
-- library).
data Doc =
    -- | A single character.  Cannot be a newline.
    Char { charContent :: !Char }
    -- | A raw Builder that constructs a string containing no
    -- newlines.  This is used to represent basic text.
  | Content {
      -- | Length of the text that gets built.
      contentLength :: !Int,
      -- | A Builder that constructs the text.
      contentString :: !Lazy.ByteString
    }
    -- | An erasable newline.
  | Line {
      -- | Whether to insert a space when undone by a group.
      lineKind :: !LineKind
    }
    -- | Concatenated documents.  An empty list here represents an empty @Doc@.
  | Cat {
      catDocs :: [Doc]
    }
    -- | Increase the nesting level of a document.
  | Nest {
      -- | Amount by which to increase nesting.
      nestLevel :: !Int,
      -- | Whether to align to the current column, or the base nesting
      -- level.
      nestAlign :: !Bool,
      -- | Whether the indentation is delayed, or takes place immediately.
      nestDelay :: !Bool,
      -- | Document whose nesting should be increased.
      nestDoc :: Doc
    }
    -- | Choose the \"best\" from among a list of options.
  | Choose {
      -- | The list of options.
      chooseOptions :: [Doc]
    }
    -- | Set graphics mode options when rendering the child @Doc@.
  | Graphics {
      -- | Graphics mode to set.
      graphicsSGR :: !Graphics,
      -- | Document to render with graphic mode.
      graphicsDoc :: Doc
    }
    deriving (Eq, Show)

instance Monoid Doc where
  mempty = empty
  mappend = beside
  mconcat = concat

-- | Graphics options for ANSI terminals.  All options are wrapped in
-- the 'Maybe' datatype, with 'Nothing' meaning \"leave this option
-- as-is\".
data Graphics =
    -- | Set options on the terminal, or keep the current setting in
    -- the case of 'Nothing'.
    Options {
      -- | Console intensity.
      consoleIntensity :: !(Maybe ConsoleIntensity),
      -- | Underlining.
      underlining :: !(Maybe Underlining),
      -- | Blinking speed.
      blinkSpeed :: !(Maybe BlinkSpeed),
      -- | Foreground color and intensity.
      foreground :: !(Maybe (Color, ColorIntensity)),
      -- | Background color and intensity.
      background :: !(Maybe (Color, ColorIntensity)),
      -- | Whether or not to swap the foreground and background.
      swapForegroundBackground :: !(Maybe Bool)
    }
    -- | Reset the terminal in this mode.
  | Default
    deriving (Ord, Eq, Show)

instance Monoid Graphics where
  mempty = Options { consoleIntensity = Nothing, underlining = Nothing,
                     swapForegroundBackground = Nothing, foreground = Nothing,
                     background = Nothing, blinkSpeed = Nothing }

  mappend Default opts = opts
  mappend opts Default = opts
  mappend Options { consoleIntensity = consIntensity1,
                    swapForegroundBackground = swap1,
                    underlining = underline1,
                    foreground = fore1,
                    background = back1,
                    blinkSpeed = blink1 }
          Options { consoleIntensity = consIntensity2,
                    swapForegroundBackground = swap2,
                    underlining = underline2,
                    foreground = fore2,
                    background = back2,
                    blinkSpeed = blink2 } =
    Options { consoleIntensity = case consIntensity1 of
                                   consIntensity @ (Just _) -> consIntensity
                                   Nothing -> consIntensity2,
              swapForegroundBackground = case swap1 of
                                           swap @ (Just _) -> swap
                                           Nothing -> swap2,
              underlining = case underline1 of
                              uline @ (Just _) -> uline
                              Nothing -> underline2,
              foreground = case fore1 of
                             fore @ (Just _) -> fore
                             Nothing -> fore2,
              background = case back1 of
                             back @ (Just _) -> back
                             Nothing -> back2,
              blinkSpeed = case blink1 of
                             blink @ (Just _) -> blink
                             Nothing -> blink2 }

instance Ord Doc where
  compare Char { charContent = c1 } Char { charContent = c2 } = compare c1 c2
  compare Char {} _ = LT
  compare _ Char {} = GT
  compare Content { contentString = str1 } Content { contentString = str2 } =
    compare str1 str2
  compare Content {} _ = LT
  compare _ Content {} = GT
  compare Line { lineKind = kind1 } Line { lineKind = kind2 } =
    compare kind1 kind2
  compare Line {} _ = LT
  compare _ Line {} = GT
  compare Cat { catDocs = docs1 } Cat { catDocs = docs2 } = compare docs1 docs2
  compare Cat {} _ = LT
  compare _ Cat {} = GT
  compare Nest { nestLevel = lvl1, nestAlign = al1,
                 nestDelay = delay1, nestDoc = doc1 }
          Nest { nestLevel = lvl2, nestAlign = al2,
                 nestDelay = delay2, nestDoc = doc2 } =
    case compare lvl1 lvl2 of
      EQ -> case compare al1 al2 of
        EQ -> case compare delay1 delay2 of
          EQ -> compare doc1 doc2
          out -> out
        out -> out
      out -> out
  compare Nest {} _ = LT
  compare _ Nest {} = GT
  compare Choose { chooseOptions = opts1 } Choose { chooseOptions = opts2 } =
    compare opts1 opts2
  compare Choose {} _ = LT
  compare _ Choose {} = GT
  compare Graphics { graphicsSGR = sgr1, graphicsDoc = doc1 }
          Graphics { graphicsSGR = sgr2, graphicsDoc = doc2 } =
    case compare sgr1 sgr2 of
      EQ -> compare doc1 doc2
      out -> out

instance Hashable LineKind where
  hashWithSalt s = hashWithSalt s . fromEnum

instance Hashable Doc where
  hashWithSalt s Char { charContent = c } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` c
  hashWithSalt s Content { contentLength = len, contentString = str } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` len `hashWithSalt` str
  hashWithSalt s Line { lineKind = kind } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` kind
  hashWithSalt s Cat { catDocs = docs } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` docs
  hashWithSalt s Nest { nestLevel = lvl, nestAlign = al,
                        nestDelay = delay, nestDoc = doc } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` lvl `hashWithSalt`
    al `hashWithSalt` delay `hashWithSalt` doc
  hashWithSalt s Choose { chooseOptions = opts } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` sort opts
  hashWithSalt s Graphics { graphicsSGR = sgr, graphicsDoc = doc } =
    s `hashWithSalt` (6 :: Int) `hashWithSalt` sgr `hashWithSalt` doc

instance Hashable Graphics where
  hashWithSalt s Options { consoleIntensity = consIntensity,
                           swapForegroundBackground = swap,
                           underlining = uline,
                           foreground = fore,
                           background = back,
                           blinkSpeed = blink } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt`
    fmap fromEnum consIntensity `hashWithSalt`
    fmap fromEnum swap `hashWithSalt` fmap fromEnum uline `hashWithSalt`
    fmap (fromEnum *** fromEnum) fore `hashWithSalt`
    fmap (fromEnum *** fromEnum) back `hashWithSalt` fmap fromEnum blink
  hashWithSalt s Default = s `hashWithSalt` (1 :: Int)

-- | Generate a 'Doc' representing a graphics mode switch.
switchGraphics :: Graphics -> Graphics -> Builder
switchGraphics _ Default = fromString (setSGRCode [Reset])
switchGraphics Default Options { consoleIntensity = consIntensity,
                                 swapForegroundBackground = swap,
                                 underlining = uline,
                                 foreground = fore,
                                 background = back,
                                 blinkSpeed = blink } =
  let
    withConsIntensity = maybe [] ((: []) . SetConsoleIntensity) consIntensity
    withUnderline = maybe withConsIntensity ((: withConsIntensity) .
                                             SetUnderlining)
                          uline
    withBlink = maybe withUnderline ((: withUnderline) . SetBlinkSpeed) blink
    withSwap = maybe withBlink ((: withBlink) . SetSwapForegroundBackground)
                     swap
    withForeground =
      maybe withSwap (\(color, intensity) -> SetColor Foreground intensity
                                             color : withSwap) fore
    withBackground =
      maybe withForeground (\(color, intensity) -> SetColor Background intensity
                                                   color : withForeground) back
  in
    fromString (setSGRCode withBackground)
switchGraphics Options { consoleIntensity = consIntensity1,
                         swapForegroundBackground = swap1,
                         underlining = underline1,
                         foreground = fore1,
                         background = back1,
                         blinkSpeed = blink1 }
               Options { consoleIntensity = consIntensity2,
                         swapForegroundBackground = swap2,
                         underlining = underline2,
                         foreground = fore2,
                         background = back2,
                         blinkSpeed = blink2 } =
  let
    withConsIntensity =
      if consIntensity1 /= consIntensity2
        then maybe [] ((: []) . SetConsoleIntensity) consIntensity2
        else []
    withUnderline =
      if underline1 /= underline2
        then maybe withConsIntensity ((: withConsIntensity) . SetUnderlining)
                   underline2
        else withConsIntensity
    withBlink =
      if blink1 /= blink2
        then maybe withUnderline ((: withUnderline) . SetBlinkSpeed) blink2
        else withUnderline
    withSwap =
      if swap1 /= swap2
        then maybe withBlink ((: withBlink) . SetSwapForegroundBackground) swap2
        else withBlink
    withForeground =
      if fore1 /= fore2
        then maybe withSwap (\(color, intensity) ->
                              SetColor Foreground intensity color : withSwap)
                   fore2
        else withSwap
    withBackground =
      if back1 /= back2
        then maybe withSwap (\(color, intensity) ->
                              SetColor Background intensity color :
                              withForeground) back2
        else withForeground
  in
    fromString (setSGRCode withBackground)

-- | An empty 'Doc'.
empty :: Doc
empty = Cat { catDocs = [] }

-- | A 'Doc' consisting of a linebreak, that is not turned into a
-- space when erased by a 'group'.
line :: Doc
line = Line { lineKind = Soft }

-- | A 'Doc' consisting of a linebreak, that is turned into a space
-- when erased by a 'group'.
linebreak :: Doc
linebreak = Line { lineKind = Break }

-- | A 'Doc' consisting of a linebreak that cannot be erased by a 'group'.
hardline :: Doc
hardline = Line { lineKind = Hard }

-- | A 'Doc' consisting of a space character, that can be turned into
-- a linebreak in order to break lines that are too long.
softline :: Doc
softline = Choose { chooseOptions = [ char ' ', linebreak ] }

-- | An empty 'Doc' that can be turned into a linebreak in order to
-- break lines that are too long.
softbreak :: Doc
softbreak = Choose { chooseOptions = [ empty, line ] }

-- | A 'Doc' containing a single character.
char :: Char -> Doc
char '\n' = line
char chr = Char { charContent = chr }

-- | Create a 'Doc' containing a string.
string :: String -> Doc
string str = Content { contentString = Lazy.UTF8.fromString str,
                       contentLength = length str }

-- | Create a 'Doc' containing a bytestring.
bytestring :: Strict.ByteString -> Doc
bytestring txt
  | Strict.null txt = empty
  | otherwise = Content { contentLength = Strict.UTF8.length txt,
                          contentString = Lazy.fromStrict txt }

-- | Create a 'Doc' containing a lazy bytestring
lazyBytestring :: Lazy.ByteString -> Doc
lazyBytestring txt
  | Lazy.null txt = empty
  | otherwise = Content { contentLength = Lazy.UTF8.length txt,
                          contentString = txt }

-- | The character @(@
lparen :: Doc
lparen = char '('

-- | The character @)@
rparen :: Doc
rparen = char ')'

-- | The character @[@
lbrack :: Doc
lbrack = char '['

-- | The character @]@
rbrack :: Doc
rbrack = char ']'

-- | The character @{@
lbrace :: Doc
lbrace = char '{'

-- | The character @}@
rbrace :: Doc
rbrace = char '}'

-- | The character @<@
langle :: Doc
langle = char '<'

-- | The character @>@
rangle :: Doc
rangle = char '>'

-- | The character @'@
squote :: Doc
squote = char '\''

-- | The character @"@
dquote :: Doc
dquote = char '"'

-- | The character @`@
backquote :: Doc
backquote = char '`'

-- | The character @,@
comma :: Doc
comma = char ','

-- | The character @;@
semi :: Doc
semi = char ';'

-- | The character @:@
colon :: Doc
colon = char ':'

-- | The character @.@
dot :: Doc
dot = char '.'

-- | The character @|@
bar :: Doc
bar = char '|'

-- | The character @\@
backslash :: Doc
backslash = char '\\'

-- | A space character.
space :: Doc
space = char ' '

-- | The character @=@
equals :: Doc
equals = char '='

-- | Increase the indentation level of a document by some amount.
nest :: Int -> Doc -> Doc
nest _ c @ Cat { catDocs = [] } = c
nest lvl n @ Nest { nestLevel = lvl' } = n { nestLevel = lvl + lvl' }
nest lvl doc = Nest { nestDelay = True, nestAlign = False,
                      nestLevel = lvl, nestDoc = doc }

-- | Increase the indentation level of a document by some amount.
indent :: Int -> Doc -> Doc
indent _ c @ Cat { catDocs = [] } = c
indent lvl n @ Nest { nestLevel = lvl' } = n { nestLevel = lvl + lvl' }
indent lvl doc = Nest { nestDelay = False, nestAlign = False,
                        nestLevel = lvl, nestDoc = doc }

-- | Set the indentation level to the current column.  This is
-- equivalent to @alignOffset 0@.
align :: Doc -> Doc
align = alignOffset 0

-- | Set the indetation level to the current column, plus some offset.
alignOffset :: Int
            -- ^ Offset to current column.  Can be negative.
            -> Doc
            -- ^ The @Doc@ to align.
            -> Doc
alignOffset offset inner = Nest { nestDelay = True, nestAlign = True,
                                  nestLevel = offset, nestDoc = inner }


-- | Enclose a 'Doc' in single quotes
squoted :: Doc -> Doc
squoted = enclose squote squote

-- | Enclose a 'Doc' in double quotes
dquoted :: Doc -> Doc
dquoted = enclose dquote dquote

-- | Enclose a 'Doc' in paretheses
parens :: Doc -> Doc
parens = enclose lparen rparen

-- | Enclose a 'Doc' in brackets
brackets :: Doc -> Doc
brackets = enclose lbrack rbrack

-- | Enclose a 'Doc' in braces
braces :: Doc -> Doc
braces = enclose lbrace rbrace

-- | Enclose a 'Doc' in angles
angles :: Doc -> Doc
angles = enclose langle rangle

-- | Set the graphics mode on a document.
graphics :: Graphics -> Doc -> Doc
graphics sgr1 Graphics { graphicsDoc = doc, graphicsSGR = sgr2 } =
  Graphics { graphicsDoc = doc, graphicsSGR = sgr1 <> sgr2 }
graphics sgr doc = Graphics { graphicsDoc = doc, graphicsSGR = sgr }

-- | Color a 'Doc' dull white.
dullWhite :: Doc -> Doc
dullWhite = graphics Options { consoleIntensity = Nothing,
                               underlining = Nothing,
                               blinkSpeed = Nothing,
                               foreground = Just (White, Dull),
                               background = Nothing,
                               swapForegroundBackground = Nothing }

-- | Color a 'Doc' dull red.
dullRed :: Doc -> Doc
dullRed = graphics Options { consoleIntensity = Nothing,
                             underlining = Nothing,
                             blinkSpeed = Nothing,
                             foreground = Just (Red, Dull),
                             background = Nothing,
                             swapForegroundBackground = Nothing }

-- | Color a 'Doc' dull yellow.
dullYellow :: Doc -> Doc
dullYellow = graphics Options { consoleIntensity = Nothing,
                                underlining = Nothing,
                                blinkSpeed = Nothing,
                                foreground = Just (Yellow, Dull),
                                background = Nothing,
                                swapForegroundBackground = Nothing }

-- | Color a 'Doc' dull green.
dullGreen :: Doc -> Doc
dullGreen = graphics Options { consoleIntensity = Nothing,
                               underlining = Nothing,
                               blinkSpeed = Nothing,
                               foreground = Just (Green, Dull),
                               background = Nothing,
                               swapForegroundBackground = Nothing }

-- | Color a 'Doc' dull blue.
dullBlue :: Doc -> Doc
dullBlue = graphics Options { consoleIntensity = Nothing,
                              underlining = Nothing,
                              blinkSpeed = Nothing,
                              foreground = Just (Blue, Dull),
                              background = Nothing,
                              swapForegroundBackground = Nothing }

-- | Color a 'Doc' dull cyan.
dullCyan :: Doc -> Doc
dullCyan = graphics Options { consoleIntensity = Nothing,
                              underlining = Nothing,
                              blinkSpeed = Nothing,
                              foreground = Just (Cyan, Dull),
                              background = Nothing,
                              swapForegroundBackground = Nothing }

-- | Color a 'Doc' dull magenta.
dullMagenta :: Doc -> Doc
dullMagenta = graphics Options { consoleIntensity = Nothing,
                                 underlining = Nothing,
                                 blinkSpeed = Nothing,
                                 foreground = Just (Magenta, Dull),
                                 background = Nothing,
                                 swapForegroundBackground = Nothing }

-- | Color a 'Doc' dull black.
dullBlack :: Doc -> Doc
dullBlack = graphics Options { consoleIntensity = Nothing,
                               underlining = Nothing,
                               blinkSpeed = Nothing,
                               foreground = Just (Black, Dull),
                               background = Nothing,
                               swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid white.
vividWhite :: Doc -> Doc
vividWhite = graphics Options { consoleIntensity = Nothing,
                                underlining = Nothing,
                                blinkSpeed = Nothing,
                                foreground = Just (White, Vivid),
                                background = Nothing,
                                swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid red.
vividRed :: Doc -> Doc
vividRed = graphics Options { consoleIntensity = Nothing,
                              underlining = Nothing,
                              blinkSpeed = Nothing,
                              foreground = Just (Red, Vivid),
                              background = Nothing,
                              swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid yellow.
vividYellow :: Doc -> Doc
vividYellow = graphics Options { consoleIntensity = Nothing,
                                 underlining = Nothing,
                                 blinkSpeed = Nothing,
                                 foreground = Just (Yellow, Vivid),
                                 background = Nothing,
                                 swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid green.
vividGreen :: Doc -> Doc
vividGreen = graphics Options { consoleIntensity = Nothing,
                                underlining = Nothing,
                                blinkSpeed = Nothing,
                                foreground = Just (Green, Vivid),
                                background = Nothing,
                                swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid blue.
vividBlue :: Doc -> Doc
vividBlue = graphics Options { consoleIntensity = Nothing,
                               underlining = Nothing,
                               blinkSpeed = Nothing,
                               foreground = Just (Blue, Vivid),
                               background = Nothing,
                               swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid cyan.
vividCyan :: Doc -> Doc
vividCyan = graphics Options { consoleIntensity = Nothing,
                               underlining = Nothing,
                               blinkSpeed = Nothing,
                               foreground = Just (Cyan, Vivid),
                               background = Nothing,
                               swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid magenta.
vividMagenta :: Doc -> Doc
vividMagenta = graphics Options { consoleIntensity = Nothing,
                                  underlining = Nothing,
                                  blinkSpeed = Nothing,
                                  foreground = Just (Magenta, Vivid),
                                  background = Nothing,
                                  swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid black.
vividBlack :: Doc -> Doc
vividBlack = graphics Options { consoleIntensity = Nothing,
                                underlining = Nothing,
                                blinkSpeed = Nothing,
                                foreground = Just (Black, Vivid),
                                background = Nothing,
                                swapForegroundBackground = Nothing }

-- | Color a 'Doc's background dull white.
dullWhiteBackground :: Doc -> Doc
dullWhiteBackground = graphics Options { consoleIntensity = Nothing,
                                         underlining = Nothing,
                                         blinkSpeed = Nothing,
                                         background = Just (White, Dull),
                                         foreground = Nothing,
                                         swapForegroundBackground = Nothing }

-- | Color a 'Doc's background dull red.
dullRedBackground :: Doc -> Doc
dullRedBackground = graphics Options { consoleIntensity = Nothing,
                                       underlining = Nothing,
                                       blinkSpeed = Nothing,
                                       background = Just (Red, Dull),
                                       foreground = Nothing,
                                       swapForegroundBackground = Nothing }

-- | Color a 'Doc's background dull yellow.
dullYellowBackground :: Doc -> Doc
dullYellowBackground = graphics Options { consoleIntensity = Nothing,
                                          underlining = Nothing,
                                          blinkSpeed = Nothing,
                                          background = Just (Yellow, Dull),
                                          foreground = Nothing,
                                          swapForegroundBackground = Nothing }

-- | Color a 'Doc's background dull green.
dullGreenBackground :: Doc -> Doc
dullGreenBackground = graphics Options { consoleIntensity = Nothing,
                                         underlining = Nothing,
                                         blinkSpeed = Nothing,
                                         background = Just (Green, Dull),
                                         foreground = Nothing,
                                         swapForegroundBackground = Nothing }

-- | Color a 'Doc's background dull blue.
dullBlueBackground :: Doc -> Doc
dullBlueBackground = graphics Options { consoleIntensity = Nothing,
                                        underlining = Nothing,
                                        blinkSpeed = Nothing,
                                        background = Just (Blue, Dull),
                                        foreground = Nothing,
                                        swapForegroundBackground = Nothing }

-- | Color a 'Doc's background dull cyan.
dullCyanBackground :: Doc -> Doc
dullCyanBackground = graphics Options { consoleIntensity = Nothing,
                                        underlining = Nothing,
                                        blinkSpeed = Nothing,
                                        background = Just (Cyan, Dull),
                                        foreground = Nothing,
                                        swapForegroundBackground = Nothing }

-- | Color a 'Doc's background dull magenta.
dullMagentaBackground :: Doc -> Doc
dullMagentaBackground = graphics Options { consoleIntensity = Nothing,
                                           underlining = Nothing,
                                           blinkSpeed = Nothing,
                                           background = Just (Magenta, Dull),
                                           foreground = Nothing,
                                           swapForegroundBackground = Nothing }

-- | Color a 'Doc's background dull black.
dullBlackBackground :: Doc -> Doc
dullBlackBackground = graphics Options { consoleIntensity = Nothing,
                                         underlining = Nothing,
                                         blinkSpeed = Nothing,
                                         background = Just (Black, Dull),
                                         foreground = Nothing,
                                         swapForegroundBackground = Nothing }

-- | Color a 'Doc's background vivid white.
vividWhiteBackground :: Doc -> Doc
vividWhiteBackground = graphics Options { consoleIntensity = Nothing,
                                          underlining = Nothing,
                                          blinkSpeed = Nothing,
                                          background = Just (White, Vivid),
                                          foreground = Nothing,
                                          swapForegroundBackground = Nothing }

-- | Color a 'Doc's background vivid red.
vividRedBackground :: Doc -> Doc
vividRedBackground = graphics Options { consoleIntensity = Nothing,
                                        underlining = Nothing,
                                        blinkSpeed = Nothing,
                                        background = Just (Red, Vivid),
                                        foreground = Nothing,
                                        swapForegroundBackground = Nothing }

-- | Color a 'Doc's background vivid yellow.
vividYellowBackground :: Doc -> Doc
vividYellowBackground = graphics Options { consoleIntensity = Nothing,
                                           underlining = Nothing,
                                           blinkSpeed = Nothing,
                                           background = Just (Yellow, Vivid),
                                           foreground = Nothing,
                                           swapForegroundBackground = Nothing }

-- | Color a 'Doc's background vivid green.
vividGreenBackground :: Doc -> Doc
vividGreenBackground = graphics Options { consoleIntensity = Nothing,
                                          underlining = Nothing,
                                          blinkSpeed = Nothing,
                                          background = Just (Green, Vivid),
                                          foreground = Nothing,
                                          swapForegroundBackground = Nothing }

-- | Color a 'Doc's background vivid blue.
vividBlueBackground :: Doc -> Doc
vividBlueBackground = graphics Options { consoleIntensity = Nothing,
                                         underlining = Nothing,
                                         blinkSpeed = Nothing,
                                         background = Just (Blue, Vivid),
                                         foreground = Nothing,
                                         swapForegroundBackground = Nothing }

-- | Color a 'Doc's background vivid cyan.
vividCyanBackground :: Doc -> Doc
vividCyanBackground = graphics Options { consoleIntensity = Nothing,
                                         underlining = Nothing,
                                         blinkSpeed = Nothing,
                                         background = Just (Cyan, Vivid),
                                         foreground = Nothing,
                                         swapForegroundBackground = Nothing }

-- | Color a 'Doc's background vivid magenta.
vividMagentaBackground :: Doc -> Doc
vividMagentaBackground = graphics Options { consoleIntensity = Nothing,
                                            underlining = Nothing,
                                            blinkSpeed = Nothing,
                                            background = Just (Magenta, Vivid),
                                            foreground = Nothing,
                                            swapForegroundBackground = Nothing }

-- | Color a 'Doc's background vivid black.
vividBlackBackground :: Doc -> Doc
vividBlackBackground = graphics Options { consoleIntensity = Nothing,
                                          underlining = Nothing,
                                          blinkSpeed = Nothing,
                                          background = Just (Black, Vivid),
                                          foreground = Nothing,
                                          swapForegroundBackground = Nothing }

-- | Set the terminal to bold.
bold :: Doc -> Doc
bold = graphics Options { consoleIntensity = Just BoldIntensity,
                          underlining = Nothing,
                          blinkSpeed = Nothing,
                          background = Nothing,
                          foreground = Nothing,
                          swapForegroundBackground = Nothing }

-- | Unset the terminal from bold.
debold :: Doc -> Doc
debold = graphics Options { consoleIntensity = Just NormalIntensity,
                            underlining = Nothing,
                            blinkSpeed = Nothing,
                            background = Nothing,
                            foreground = Nothing,
                            swapForegroundBackground = Nothing }

-- | Unset the terminal from bold.
underline :: Doc -> Doc
underline = graphics Options { consoleIntensity = Nothing,
                               underlining = Just SingleUnderline,
                               blinkSpeed = Nothing,
                               background = Nothing,
                               foreground = Nothing,
                               swapForegroundBackground = Nothing }

-- | Unset the terminal from bold.
deunderline :: Doc -> Doc
deunderline = graphics Options { consoleIntensity = Nothing,
                                 underlining = Just NoUnderline,
                                 blinkSpeed = Nothing,
                                 background = Nothing,
                                 foreground = Nothing,
                                 swapForegroundBackground = Nothing }

-- | Join two 'Doc's with a space in between them.
(<+>) :: Doc -> Doc -> Doc
left <+> right = concat [left, space, right]

-- | Join two 'Doc's with a 'hardline' in between them.
(<!>) :: Doc -> Doc -> Doc
left <!> right = concat [left, hardline, right]

-- | Join two 'Doc's with a 'line' in between them.
(<$>) :: Doc -> Doc -> Doc
left <$> right = concat [left, line, right]

-- | Join two 'Doc's with a 'linebreak' in between them.
(<$$>) :: Doc -> Doc -> Doc
left <$$> right = concat [left, linebreak, right]

-- | Join two 'Doc's with a 'softline' in between them.
(</>) :: Doc -> Doc -> Doc
left </> right = concat [left, softline, right]

-- | Join two 'Doc's with a 'softbreak' in between them.
(<//>) :: Doc -> Doc -> Doc
left <//> right = concat [left, softbreak, right]

-- | Joun 'Doc's with no space in between them.
beside :: Doc -> Doc -> Doc
beside Cat { catDocs = left } Cat { catDocs = right } =
  Cat { catDocs = left ++ right }
beside left Cat { catDocs = right } = Cat { catDocs = left : right }
beside Cat { catDocs = left } right = Cat { catDocs = left ++ [right] }
beside left right = Cat { catDocs = [left, right] }

-- | Concatenate a list of 'Doc's.  This is generally more efficient
-- than repeatedly using 'beside' or '<>'.
concat :: [Doc] -> Doc
concat docs = Cat { catDocs = docs }

-- | A choice of several options.  Only one of these will be chosen
-- and used to render the final document.
choose :: [Doc] -> Doc
choose [] = empty
choose [doc] = doc
choose docs = Choose { chooseOptions = docs }

-- | Concatenate a list of 'Doc's.  This is generally more efficient
-- than repeatedly using 'beside' or '<>'.
hcat :: [Doc] -> Doc
hcat docs = Cat { catDocs = docs }

-- | Join a list of 'Doc's with spaces in between each.  This is
-- generally more efficient than repeatedly using '<+>'.
hsep :: [Doc] -> Doc
hsep = concat . intersperse space

-- | Join a list of 'Doc's with 'line's in between each.  This is
-- generally more efficient than repeatedly using '<$$>'.
vsep :: [Doc] -> Doc
vsep = concat . intersperse line

-- | Join a list of 'Doc's with 'linebreak's in between each.  This is
-- generally more efficient than repeatedly using '<$>'.
vcat :: [Doc] -> Doc
vcat = concat . intersperse linebreak

-- | Join a list of 'Doc's using either 'hsep' or 'vsep'.
sep :: [Doc] -> Doc
sep docs = Choose { chooseOptions = [hsep docs, vsep docs] }

-- | Join a list of 'Doc's using either 'hcat' or 'vcat'.
cat :: [Doc] -> Doc
cat docs = Choose { chooseOptions = [hcat docs, vcat docs] }

-- | Join a list of 'Doc's with 'softline's in between each.  This is
-- generally more efficient than repeatedly using '</>'.
fillSep :: [Doc] -> Doc
fillSep = concat . intersperse softline

-- | Join a list of 'Doc's with 'softbreak's in between each.  This is
-- generally more efficient than repeatedly using '<//>'.
fillCat :: [Doc] -> Doc
fillCat = concat . intersperse softbreak

-- | Enclose a 'Doc' within two other 'Doc's
enclose :: Doc -> Doc -> Doc -> Doc
enclose left right middle = hcat [left, middle, right]

-- | Concatenate a list of 'Doc's into a single doc, with each element
-- separated from the others by a given 'Doc' representing
-- punctuation.
punctuate :: Doc -> [Doc] -> [Doc]
punctuate punc (first : rest @ (_ : _)) = first <> punc : punctuate punc rest
punctuate _ doc = doc

-- | Enclose a list of 'Doc's, separated by punctuation, and align
-- nesting of the contents to the end of the left enclosing 'Doc'
encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep left right _ [] = left <> right
encloseSep left right _ [doc] = left <> doc <> right
encloseSep left right middle docs =
  left <> align (concat (punctuate middle docs)) <> right

-- | Render a list, enclosed in brackets and separated by commas.
list :: [Doc] -> Doc
list = group . encloseSep lbrack rbrack (comma <> line)

-- | Erase all linebreaks in a 'Doc' and replace them with either
-- spaces or nothing, depending on the kind of linebreak.
flatten :: Doc -> Maybe Doc
flatten Line { lineKind = Hard } = Nothing
flatten Line { lineKind = Break } = Just Char { charContent = ' ' }
flatten Line { lineKind = Soft } = Just empty
flatten Cat { catDocs = docs } =
  case mapMaybe flatten docs of
    [] -> Nothing
    flatinner -> Just Cat { catDocs = flatinner }
flatten Choose { chooseOptions = docs } =
  case mapMaybe flatten docs of
    [] -> Nothing
    flatdocs -> Just Choose { chooseOptions = flatdocs }
flatten n @ Nest { nestDoc = inner } =
  do
    flatinner <- flatten inner
    return n { nestDoc = flatinner }
flatten doc = Just doc

-- | A 'Doc' that 'choose's between the unmodified argument, or the
-- 'flatten'ed version of the argument.
group :: Doc -> Doc
group doc = case flatten doc of
  Just flatdoc -> Choose { chooseOptions = [ doc, flatdoc ] }
  Nothing -> doc

-- | Produce a 'Builder' that renders the 'Doc' to one line.
buildOneLine :: Doc -> Builder
buildOneLine Char { charContent = chr } = fromChar chr
buildOneLine Content { contentString = builder } = fromLazyByteString builder
buildOneLine Line { lineKind = Break } = fromChar ' '
buildOneLine Line { lineKind = Soft } = mempty
buildOneLine Line { lineKind = Hard } = fromChar '\n'
buildOneLine Cat { catDocs = docs } = mconcat (map buildOneLine docs)
buildOneLine Nest { nestDoc = inner } = buildOneLine inner
buildOneLine Choose { chooseOptions = opts } = buildOneLine (head opts)
buildOneLine Graphics { graphicsDoc = inner } = buildOneLine inner

-- | Render the entire 'Doc' to one line.  Good for output that
-- will be read only by a machine, where newlines are not important at all
renderOneLine :: Doc -> Lazy.ByteString
renderOneLine = toLazyByteString . buildOneLine

-- | Output the entire 'Doc', as rendered by 'renderOneLine' to the
-- given 'Handle'.
putOneLine :: Handle -> Doc -> IO ()
putOneLine handle =
  toByteStringIO (Strict.hPut handle) . buildOneLine

-- | Produce a 'Builder' that renders the 'Doc' quickly.
buildFast :: Doc -> Builder
buildFast Char { charContent = chr } = fromChar chr
buildFast Content { contentString = builder } = fromLazyByteString builder
buildFast Line {} = fromChar '\n'
buildFast Cat { catDocs = docs } = mconcat (map buildFast docs)
buildFast Nest { nestDoc = inner } = buildFast inner
buildFast Choose { chooseOptions = opts } = buildFast (head opts)
buildFast Graphics { graphicsDoc = inner } = buildFast inner

-- | Render the entire 'Doc', preserving newlines, but without any
-- indentation.  Good for output that will be read only by machine,
-- but where newlines matter.
renderFast :: Doc -> Lazy.ByteString
renderFast = toLazyByteString . buildFast

-- | Output the entire 'Doc', as rendered by 'renderFast' to the
-- given 'Handle'.
putFast :: Handle -> Doc -> IO ()
putFast handle =
  toByteStringIO (Strict.hPut handle) . buildFast

-- | A description of the ending.
data Ending =
    -- | Ended with a newline, so do a full indent.
    Indent
    -- | Ended with normal content, so no indent.
  | None
    deriving (Eq, Show)

-- | A rendering of a document.
data Render =
  Render {
    -- | Number of extra spaces to generate.
    renderNesting :: !Int,
    -- | Width: Number of columns at the widest point in the complete
    -- lines.
    renderWidth :: !Int,
    -- | Ending line fragment width.
    renderEndWidth :: !Int,
    -- | The number of lines in the document.
    renderLines :: !Word,
    -- | A builder that constructs the document.
    renderBuilder :: Builder,
    -- | Indentation mode for the next document.
    renderEnding :: !Ending,
    -- | Finish function.  Allows for resetting of some state when
    -- done with an inner document.
    renderFinish :: !(Render -> Render)
  }

instance Show Render where
  show Render { renderWidth = mwidth, renderEndWidth = ewidth,
                renderLines = lns, renderBuilder = builder,
                renderEnding = end, renderNesting = nesting } =
    "Render { renderNesting = " ++ show nesting ++
    ", renderWidth = " ++ show mwidth ++
    ", renderEndWidth = " ++ show ewidth ++
    ", renderLines = " ++ show lns ++
    ", renderEnding = " ++ show end ++
    ", renderBuilder = " ++ show (toLazyByteString builder) ++
    " }"

-- | Determine whether the first 'Render' is strictly better than the second.
subsumes :: Render -> Render -> Bool
-- Simple comparisons: if the width is subsumed and the lines are less
subsumes Render { renderWidth = width1, renderLines = lines1,
                  renderEndWidth = ewidth1 }
         Render { renderWidth = width2, renderLines = lines2,
                  renderEndWidth = ewidth2 }=
  lines1 <= lines2 && width1 <= width2 && ewidth1 <= ewidth2

newtype Frontier = Frontier { frontierRenders :: [Render] }
  deriving Show

instance Monoid Frontier where
  mempty = Frontier { frontierRenders = [] }

  mappend front1 @ Frontier { frontierRenders = renders1 }
          front2 @ Frontier { frontierRenders = renders2 } =
    if length renders1 < length renders2
      then foldl insertRender front2 renders1
      else foldl insertRender front1 renders2

mapFrontier :: (Render -> Render) -> Frontier -> Frontier
mapFrontier func f @ Frontier { frontierRenders = renders } =
  f { frontierRenders = map func renders }

-- Add a 'Render' into a result set, ensuring that any subsumed
-- renders are dropped.
--
-- TODO: The asymptotic runtime of this could likely be improved by some
-- kind of tree structure; however, the design of this structure is
-- nontrivial, due to the 3+-dimensional nature of subsumption, and
-- the wierd interactions between the various kinds of column offsets.
insertRender :: Frontier -> Render -> Frontier
insertRender Frontier { frontierRenders = renders } ins
    -- If the inserted element is subsumed by anything in the
    -- list, then don't insert it at all.
  | any (`subsumes` ins) renders = Frontier { frontierRenders = renders }
    -- Otherwise, add the element to the list, and drop everything it subsumes.
  | otherwise =
    Frontier { frontierRenders = ins : filter (not . subsumes ins) renders }

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
    deriving Show

mapResult :: (Render -> Render) -> Result -> Result
mapResult func Single { singleRender = render } =
  Single { singleRender = func render }
mapResult func Multi { multiOptions = opts } =
  Multi { multiOptions = mapFrontier func opts }

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
mergeResults s1 @ Single { singleRender = r1 }
             s2 @ Single { singleRender = r2 }
  | subsumes r1 r2 = s1
  | subsumes r2 r1 = s2
  | otherwise = Multi { multiOptions = Frontier { frontierRenders = [r1, r2] } }
mergeResults Single { singleRender = render }
             Multi { multiOptions = opts } =
  packResult (insertRender opts render)
-- This operation is commutative
mergeResults m @ Multi {} s @ Single {} = mergeResults s m
-- Otherwise it's a straightaway HashMap union
mergeResults Multi { multiOptions = opts1 } Multi { multiOptions = opts2 } =
  packResult (opts1 <> opts2)

-- | Produce a 'Builder' that renders the 'Doc' using the optimal
-- layout engine.

appendContent :: Int -> Builder -> Render -> Render
appendContent len content r @ Render { renderEnding = Indent,
                                       renderNesting = lvl,
                                       renderBuilder = builder,
                                       renderEndWidth = ewidth } =
  let
    ind = max 0 (lvl - ewidth)
  in
    r { renderBuilder = builder <> makespaces ind <> content,
        renderEnding = None, renderEndWidth = ewidth + ind + len }
appendContent len content r @ Render { renderBuilder = builder,
                                       renderEndWidth = ewidth } =
  r { renderBuilder = builder <> content,
      renderEnding = None, renderEndWidth = ewidth + len }

appendLine :: Render -> Render
appendLine r @ Render { renderBuilder = builder, renderLines = l,
                        renderWidth = width, renderEndWidth = ewidth } =
  r { renderBuilder = builder <> fromChar '\n', renderWidth = max width ewidth,
      renderLines = l + 1, renderEnding = Indent, renderEndWidth = 0 }

-- | Core algorithm.  Walks forward in the Doc, maintaining a dynamic
-- programming frontier consisting of all renders that aren't subsumed
-- by other renders (possibly with a limit on the size)
buildDynamic :: Maybe Int
             -- ^ The maximum frontier size
             -> Int
             -- ^ The maximum number of columns.
             -> Bool
             -- ^ Whether or not to render with ANSI terminal options.
             -> Doc
             -- ^ The document to render.
             -> Builder
buildDynamic fsize maxcol ansiterm doc =
  let
    build :: Graphics -> Result -> [Doc] -> Result
    build _ accum [] = mapResult (\r @ Render { renderFinish = f } -> f r) accum
    build _ accum d
      | debug ("build\n  " ++ show accum ++ "\n  " ++ show d ++ " =") False =
        undefined
    -- For content, append to the accumulated values and continue.
    build sgr accum (Char { charContent = chr } : rest) =
      let
        withcontent = mapResult (appendContent 1 (fromChar chr)) accum
      in
        build sgr withcontent rest
    build sgr accum (Content { contentString = txt,
                               contentLength = len } : rest) =
      let
        withcontent =
          mapResult (appendContent len (fromLazyByteString txt)) accum
      in
        build sgr withcontent rest
    build sgr accum (Line {} : rest) =
      let
        withcontent = mapResult appendLine accum
      in
        build sgr withcontent rest
    -- Build the inner docs, then continue
    build sgr accum (Cat { catDocs = docs } : rest) =
      build sgr accum (docs ++ rest)
    -- Non-aligning, delayed indent
    build sgr accum (Nest { nestDelay = True, nestAlign = False,
                            nestDoc = inner, nestLevel = off } : rest) =
      let
        setNesting r @ Render { renderFinish = oldfinish,
                                renderNesting = lvl  } =
          let
            finish r' = r' { renderNesting = lvl, renderFinish = oldfinish }
          in
            r { renderNesting = lvl + off, renderFinish = finish }
      in
        build sgr (build sgr (mapResult setNesting accum) [inner]) rest
    -- Non-aligning, immediate indent
    build sgr accum (Nest { nestDelay = False, nestAlign = False,
                            nestDoc = inner, nestLevel = off } : rest) =
      let
        setNesting r @ Render { renderFinish = oldfinish,
                                renderNesting = lvl  } =
          let
            finish r' = r' { renderNesting = lvl, renderFinish = oldfinish }
          in
            r { renderNesting = lvl + off, renderFinish = finish,
                renderEnding = Indent }
      in
        build sgr (build sgr (mapResult setNesting accum) [inner]) rest
    -- Aligning, delayed indent
    build sgr accum (Nest { nestDelay = True, nestAlign = True,
                            nestDoc = inner, nestLevel = off } : rest) =
      let
        setNesting r @ Render { renderFinish = oldfinish, renderNesting = lvl,
                                renderEndWidth = ewidth } =
          let
            finish r' = r' { renderNesting = lvl, renderFinish = oldfinish }
          in
            r { renderNesting = ewidth + off, renderFinish = finish }
      in
        build sgr (build sgr (mapResult setNesting accum) [inner]) rest
    -- Aligning, immediate indent
    build sgr accum (Nest { nestDelay = False, nestAlign = True,
                            nestDoc = inner, nestLevel = off } : rest) =
      let
        setNesting r @ Render { renderFinish = oldfinish, renderNesting = lvl,
                                renderEndWidth = ewidth } =
          let
            finish r' = r' { renderNesting = lvl, renderFinish = oldfinish }
          in
            r { renderNesting = ewidth + off, renderFinish = finish,
                renderEnding = Indent }
      in
        build sgr (build sgr (mapResult setNesting accum) [inner]) rest
    -- For choose, render each choice, then fold all the choices
    -- together and combine them into a single frontier.
    build sgr accum (Choose { chooseOptions = options } : rest) =
      let
        setRestore r @ Render { renderFinish = oldfinish } =
          let
            finish r' = r' { renderFinish = oldfinish }
          in
            r { renderFinish = finish }

        withrestore = mapResult setRestore accum

        -- Build up all the components
        results = map (build sgr withrestore . (: [])) options
        -- Now merge them into an minimal set of options
        merged = foldl1 mergeResults results
        -- Reduce the results by some amount
        reduced = case fsize of
          Just fsize' -> reduce fsize' merged
          Nothing -> merged
      in
        build sgr reduced rest
    -- For graphics, generate the control sequences before and after
    -- the inner document.
    build sgr1 accum (Graphics { graphicsDoc = inner,
                                 graphicsSGR = sgr2 } : rest)
      -- Only do graphics if the ansiterm flag is set.
      | ansiterm =
        let
          appendControl :: Render -> Render
          appendControl r @ Render { renderBuilder = builder,
                                     renderFinish = oldfinish } =
            let
              finish r' @ Render { renderBuilder = builder' } =
                r' { renderBuilder = builder' <> switchGraphics sgr2 sgr1,
                    renderFinish = oldfinish }
            in
              r { renderBuilder = builder <> switchGraphics sgr1 sgr2,
                  renderFinish = finish }
        in
          build sgr1 (build sgr2 (mapResult appendControl accum) [inner]) rest
      -- Otherwise, skip it entirely
      | otherwise = build sgr1 accum (inner : rest)

    -- | Calculate the overrun of a width.
    overrun :: Int -> Int
    overrun col = max 0 (col - maxcol)

    -- | Compare two Renders.  Less than means better.
    compareRenders Render { renderLines = lines1, renderWidth = width1,
                            renderEndWidth = ewidth1 }
                   Render { renderLines = lines2, renderWidth = width2,
                            renderEndWidth = ewidth2  } =
      let
        width1' = max width1 ewidth1
        width2' = max width2 ewidth2
      in case compare (overrun width1') (overrun width2') of
         EQ -> compare lines1 lines2
         out -> out

    -- Pick the best result out of a set of results.  Used at the end to
    -- pick the final result.
    bestRenderInOpts :: [Render] -> Render
    bestRenderInOpts = minimumBy compareRenders

    reduce :: Int -> Result -> Result
    reduce _ s @ Single {} = s
    reduce size Multi { multiOptions = Frontier { frontierRenders = opts } } =
      let
        sorted = sortBy compareRenders opts
      in
        Multi { multiOptions = Frontier { frontierRenders = take size sorted } }

    start = Render { renderWidth = 0, renderEnding = Indent, renderLines = 0,
                     renderBuilder = mempty, renderFinish = id,
                     renderEndWidth = 0, renderNesting = 0 }

    -- Call build, get the result, then pick the best one.
    Render { renderBuilder = result } =
      case build Default Single { singleRender = start } [doc] of
        Single { singleRender = render } -> render
        Multi { multiOptions = Frontier { frontierRenders = opts } } ->
          bestRenderInOpts opts
  in
    result

-- | Render a 'Doc' as a 'Builder' using an optimal layout rendering
-- engine.  The engine will render the document in the fewest number
-- of lines possible without exceeding the maximum column width.
buildOptimal :: Int
             -- ^ The maximum number of columns.
             -> Bool
             -- ^ Whether or not to render with ANSI terminal options.
             -> Doc
             -- ^ The document to render.
             -> Builder
buildOptimal = buildDynamic Nothing

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

-- | Render a 'Doc' as a 'Builder' using an optimal layout engine with
-- a constrained frontier size.  Larger frontier sizes will result in
-- a better rendering, at the cost of more execution time.  As the
-- frontier size increases, this algorithm's behavior will converge
-- towards 'renderOptimal'.
buildGreedy :: Word
            -- ^ The maximum frontier size
            -> Int
            -- ^ The maximum number of columns.
            -> Bool
            -- ^ Whether or not to render with ANSI terminal options.
            -> Doc
            -- ^ The document to render.
            -> Builder
buildGreedy 0 = error "Frontier size cannot be zero"
buildGreedy fsize = buildDynamic $! Just $! fromIntegral fsize

-- | Render a 'Doc' as a lazy bytestring using an optimal layout engine with
-- a constrained frontier size.  Larger frontier sizes will result in
-- a better rendering, at the cost of more execution time.  As the
-- frontier size increases, this algorithm's behavior will converge
-- towards 'renderOptimal'.
renderGreedy :: Word
            -- ^ The maximum frontier size
            -> Int
            -- ^ The maximum number of columns.
            -> Bool
            -- ^ Whether or not to render with ANSI terminal options.
            -> Doc
            -- ^ The document to render.
            -> Lazy.ByteString
renderGreedy fsize cols color = toLazyByteString . buildGreedy fsize cols color

-- | Output the entire 'Doc', as rendered by 'renderOptimal' to the
-- given 'Handle'.
putGreedy :: Handle
          -- ^ The 'Handle' to which to write output
          -> Word
          -- ^ The maximum frontier size
          -> Int
          -- ^ The maximum number of columns.
          -> Bool
          -- ^ Whether or not to render with ANSI terminal options.
          -> Doc
          -- ^ The document to render.
          -> IO ()
putGreedy handle fsize cols color =
  toByteStringIO (Strict.hPut handle) . buildGreedy fsize cols color

-- | A class representing datatypes that can be formatted as 'Doc's.
class Format item where
  -- | Format an @item@ as a 'Doc'
  format :: item -> Doc

  -- | Format a list of @item@s as a 'Doc'
  formatList :: [item] -> Doc
  formatList = list . map format

-- | A class representing datatypes that can be formatted as 'Doc's
-- inside a monad.
class Monad m => FormatM m item where
  -- | Format an @item@ as a 'Doc' inside an @m@ monad
  formatM :: item -> m Doc

  -- | Format a list of @item@s as a 'Doc' inside an @m@ monad
  formatListM :: [item] -> m Doc
  formatListM = liftM list . mapM formatM

instance Format a => Format [a] where
  format = formatList

instance Format Doc where
  format = id

instance Format String where
  format = string

instance Format Strict.ByteString where
  format = bytestring

instance Format Lazy.ByteString where
  format = lazyBytestring

instance Format Int where
  format = string . show

instance Format Integer where
  format = string . show

instance Format Word where
  format = string . show

instance Format Float where
  format = string . show

instance Format Double where
  format = string . show
