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
       putOptimal
       ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Arrow((***))
import Control.Monad
import Data.Hashable
import Data.List(intersperse, minimumBy, sort)
import Data.Maybe
import Data.Monoid hiding ((<>))
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
                              underline @ (Just _) -> underline
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
                           underlining = underline,
                           foreground = fore,
                           background = back,
                           blinkSpeed = blink } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt`
    fmap fromEnum consIntensity `hashWithSalt`
    fmap fromEnum swap `hashWithSalt` fmap fromEnum underline `hashWithSalt`
    fmap (fromEnum *** fromEnum) fore `hashWithSalt`
    fmap (fromEnum *** fromEnum) back `hashWithSalt` fmap fromEnum blink
  hashWithSalt s Default = s `hashWithSalt` (1 :: Int)

-- | Generate a 'Doc' representing a graphics mode switch.
switchGraphics :: Graphics -> Graphics -> Builder
switchGraphics _ Default = fromString (setSGRCode [Reset])
switchGraphics Default Options { consoleIntensity = consIntensity,
                                 swapForegroundBackground = swap,
                                 underlining = underline,
                                 foreground = fore,
                                 background = back,
                                 blinkSpeed = blink } =
  let
    withConsIntensity = maybe [] ((: []) . SetConsoleIntensity) consIntensity
    withUnderline = maybe withConsIntensity ((: withConsIntensity) .
                                             SetUnderlining)
                          underline
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

-- | Join two 'Doc's with no space in between.
(<>) :: Doc -> Doc -> Doc
(<>) = beside

-- | Join two 'Doc's with a space in between them.
(<+>) :: Doc -> Doc -> Doc
left <+> right = left <> space <> right

-- | Join two 'Doc's with a 'hardline' in between them.
(<!>) :: Doc -> Doc -> Doc
left <!> right = left <> hardline <> right

-- | Join two 'Doc's with a 'line' in between them.
(<$>) :: Doc -> Doc -> Doc
left <$> right = left <> line <> right

-- | Join two 'Doc's with a 'linebreak' in between them.
(<$$>) :: Doc -> Doc -> Doc
left <$$> right = left <> linebreak <> right

-- | Join two 'Doc's with a 'softline' in between them.
(</>) :: Doc -> Doc -> Doc
left </> right = left <> softline <> right

-- | Join two 'Doc's with a 'softbreak' in between them.
(<//>) :: Doc -> Doc -> Doc
left <//> right = left <> softbreak <> right

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

instance Eq Width where
  c1 == c2 = compare c1 c2 == EQ

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

advanceWidth :: Width -> Width -> Width
-- If the second ending width is fixed, then it doesn't change
advanceWidth _ f @ Fixed {} = f
-- If the first is fixed and the second is relative, then
-- advance the first by the relative offset, unless the first
-- document ends with a line.
advanceWidth Fixed { fixedOffset = start } Relative { relOffset = n } =
  Fixed { fixedOffset = start + n }
-- If the first is fixed and the second is a maximum, then we can
-- figure out which is the larger.
advanceWidth Fixed { fixedOffset = start } Maximum { maxFixed = fixed,
                                                     maxRelative = rel } =
  Fixed { fixedOffset = max fixed (start + rel) }
-- If both are relative, just add them and make a new relative.
advanceWidth Relative { relOffset = start } Relative { relOffset = n } =
  Relative { relOffset = start + n }
-- If we combine a relative and a maximum, then add the relative
-- offset to the relative portion of the maximum
advanceWidth Relative { relOffset = start } Maximum { maxFixed = fixed,
                                                      maxRelative = n } =
  maximum fixed (start + n)
advanceWidth Maximum { maxFixed = fixed, maxRelative = rel }
             Relative { relOffset = n } =
  maximum fixed (rel + n)
-- If both are a maximum, then the resulting relative portion is the
-- sum of the two relative portions.  The resulting fixed portion is
-- the greater of the second fixed portion, or the first fixed portion
-- plus the second relative portion.
advanceWidth Maximum { maxFixed = fixed1, maxRelative = rel1 }
             Maximum { maxFixed = fixed2, maxRelative = rel2 } =
  maximum (max fixed2 (fixed1 + rel2)) (rel1 + rel2)


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

instance Monoid Begin where
  mempty = Next

  mappend Next begin = begin
  mappend begin _ = begin

-- | A rendering of a document.
data Render =
  Render {
    -- | Starting line fragment width.
    renderStartWidth :: !Int,
    -- | Number of extra spaces to generate.
    renderNesting :: !Int,
    -- | Minimum size of the first line.
    renderMin :: !Int,
    -- | Width: Number of columns at the widest point in the complete
    -- lines.
    renderWidth :: !Width,
    -- | Ending line fragment width.
    renderEndWidth :: !Width,
    -- | The number of lines in the document.
    renderLines :: !Word,
    -- | A builder that constructs the document.
    renderBuilder :: !(Graphics -> Int -> Int -> Builder),
    -- | Indentation mode for the next document.
    renderEnding :: !Ending,
    -- | Whether or not the builder needs indentation inserted before.
    renderBegin :: !Begin
  }

instance Show Render where
  show Render { renderWidth = mwidth, renderStartWidth = swidth,
                renderEndWidth = ewidth, renderLines = lns,
                renderBuilder = builder, renderEnding = end,
                renderBegin = begin, renderNesting = nesting,
                renderMin = minwidth } =
    "Render { renderStartWidth = " ++ show swidth ++
    ", renderNesting = " ++ show nesting ++
    ", renderMin = " ++ show minwidth ++
    ", renderWidth = " ++ show mwidth ++
    ", renderEndWidth = " ++ show ewidth ++
    ", renderLines = " ++ show lns ++
    ", renderEnding = " ++ show end ++
    ", renderBegin = " ++ show begin ++
    ", renderBuilder = " ++ show (toLazyByteString (builder Default 0 0)) ++
    " }"

instance Monoid Render where
  mempty = Render { renderStartWidth = 0, renderWidth = Relative 0,
                    renderEndWidth = Relative 0, renderEnding = mempty,
                    renderNesting = 0, renderLines = 0, renderBegin = mempty,
                    renderBuilder = const mempty, renderMin = 0 }
  mappend r1 r2
    | debug ("append\n  " ++ show r1 ++ "\n  " ++ show r2 ++ " =") False =
      undefined
  mappend r1 @ Render { renderEnding = Newline }
          r2 @ Render { renderBegin = Indent, renderNesting = nest2,
                        renderMin = min2 }
    | nest2 > 0 =
      let
        builder = const $! const $! const $! makespaces nest2
        newmin = max 0 (min2 - nest2)
        spaces = Render { renderEnding = Normal, renderStartWidth = nest2,
                          renderBegin = Indent, renderWidth = Relative nest2,
                          renderLines = 0, renderBuilder = builder,
                          renderNesting = 0, renderMin = 0,
                          renderEndWidth = Relative nest2 }
    in
      r1 `mappend` spaces `mappend` r2 { renderNesting = 0, renderMin = newmin }
  mappend Render { renderBuilder = build1, renderEnding = end1,
                   renderBegin = begin1, renderNesting = nest1,
                   renderLines = lines1, renderStartWidth = swidth1,
                   renderWidth = width1, renderEndWidth = ewidth1,
                   renderMin = min1 }
          Render { renderBuilder = build2, renderEnding = end2,
                   renderBegin = begin2, renderMin = min2,
                   renderLines = lines2, renderStartWidth = swidth2,
                   renderWidth = width2, renderEndWidth = ewidth2 } =
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

      newewidth = advanceWidth ewidth1 ewidth2
      newwidth2 = advanceWidth ewidth1 width2

      -- The new width is the maximum of the two max width, the second
      -- end width, and the width of the newly-formed middle line.
      newwidth = maxWidth (maxWidth newewidth midline)
                          (maxWidth width1 newwidth2)

      out = Render { renderBuilder = newbuild, renderEnding = newend,
                     renderBegin = newbegin, renderLines = lines1 + lines2,
                     renderStartWidth = newswidth, renderWidth = newwidth,
                     renderEndWidth = newewidth, renderNesting = nest1,
                     renderMin = newmin }
    in
      debug ("    " ++ show out ++ "\n\n") out

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
subsumes :: Render -> Render -> Bool
-- Simple comparisons: if the width is subsumed and the lines are less
subsumes Render { renderWidth = width1, renderStartWidth = swidth1,
                  renderLines = lines1, renderEndWidth = ewidth1,
                  renderMin = min1 }
         Render { renderWidth = width2, renderStartWidth = swidth2,
                  renderLines = lines2, renderEndWidth = ewidth2,
                  renderMin = min2 } =
  lines1 <= lines2 && swidth1 <= swidth2 && min1 <= min2 &&
  subsumesWidth width1 width2 && subsumesWidth ewidth1 ewidth2

newtype Frontier = Frontier { frontierRenders :: [Render] }

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
      in
        -- Single characters have a single possibility, a relative
        -- ending position one beyond the start, and an upper-bound
        -- one shorter than the maximum width.
        Single { singleRender =
                    Render { renderEnding = Normal, renderBuilder = builder,
                             renderLines = 0, renderStartWidth = 1,
                             renderWidth = Relative 1, renderNesting = 0,
                             renderEndWidth = Relative 1, renderMin = 0,
                             renderBegin = Indent } }
    build Content { contentString = txt, contentLength = len } =
      let
        builder = const $! const $! const $! fromLazyByteString txt
      in
        -- Text has a single possibility and a relative ending position
        -- equal to its length
       Single { singleRender =
                    Render { renderEnding = Normal, renderBuilder = builder,
                             renderStartWidth = len, renderNesting = 0,
                             renderWidth = Relative len, renderBegin = Indent,
                             renderLines = 0, renderMin = 0,
                             renderEndWidth = Relative len } }
    build Line {} =
      let
        builder = const $! const $! const $! fromChar '\n'
      in
        -- A newline starts at the nesting level, has no overrun, and an
        -- upper-bound equal to the maximum column width.
        --
        -- Note: the upper bound is adjusted elsewhere.
        Single { singleRender =
                    Render { renderLines = 1, renderNesting = 0,
                             renderEnding = Newline, renderBegin = None,
                             renderBuilder = builder, renderStartWidth = 0,
                             renderMin = 0, renderWidth = Fixed 0,
                             renderEndWidth = Fixed 0 } }
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
        updateRender r @ Render { renderBuilder = builder,
                                  renderStartWidth = swidth,
                                  renderMin = minwidth,
                                  renderWidth = width,
                                  renderNesting = nesting,
                                  renderEndWidth = ewidth,
                                  renderBegin = begin } =
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
            r { renderBuilder = newbuilder, renderNesting = newnesting,
                renderStartWidth = newswidth, renderWidth = nestWidth width,
                renderEndWidth = nestWidth ewidth, renderMin = newmin }

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
        compareRenders Render { renderLines = lines1, renderWidth = width1 }
                       Render { renderLines = lines2, renderWidth = width2 } =
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
