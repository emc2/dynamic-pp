-- Copyright (c) 2014 Eric McCorkle.  All rights reserved.
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
       buildOneLine,
       renderOneLine,
       buildFast,
       renderFast,
       putFast,
       buildOptimal,
       renderOptimal,
       putOptimal
       ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Monad
import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Data.List(intersperse, minimumBy)
import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.Word
import Prelude hiding (concat)
import System.Console.ANSI
import System.IO

import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict.UTF8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import qualified Data.ByteString.Lazy.UTF8 as Lazy.UTF8
import qualified Data.HashMap.Strict as HashMap

-- | Datatype representing a formatted document.
data Doc =
    -- | A single character.  Cannot be a newline
    Char { charContent :: !Char }
    -- | A ByteString.
  | Bytestring {
      -- | The length of the text
      bsLength :: !Int,
      -- | The content of the text
      bsContent :: !Strict.ByteString
    }
    -- | A Lazy ByteString
  | LazyBytestring {
      -- | The length of the text
      lbsLength :: !Int,
      -- | The content of the text
      lbsContent :: !Lazy.ByteString
    }
    -- | A line.
  | Line {
      -- | Whether to insert a space when undone by a group.
      insertSpace :: !Bool
    }
    -- | Concatenated documents.
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
    -- | Choose the best from among a list of options.
  | Choose {
      -- | The list of options.
      chooseOptions :: [Doc]
    }
    -- | Set graphics mode options when rendering this doc.
  | Graphics {
      -- | Graphics mode to set.
      graphicsSGR :: !Graphics,
      -- | Document to render with graphic mode.
      graphicsDoc :: Doc
    }

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
line = Line { insertSpace = False }

-- | A 'Doc' consisting of a linebreak, that is turned into a space
-- when erased by a 'group'.
linebreak :: Doc
linebreak = Line { insertSpace = True }

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
string str =
  let
    len = length str
    bstr = Strict.UTF8.fromString str
  in
    Bytestring { bsLength = len, bsContent = bstr }

-- | Create a 'Doc' containing a bytestring.
bytestring :: Strict.ByteString -> Doc
bytestring txt
  | Strict.null txt = empty
  | otherwise = Bytestring { bsLength = Strict.UTF8.length txt,
                             bsContent = txt }

-- | Create a 'Doc' containing a lazy bytestring
lazyBytestring :: Lazy.ByteString -> Doc
lazyBytestring txt
  | Lazy.null txt = empty
  | otherwise = LazyBytestring { lbsLength = Lazy.UTF8.length txt,
                                 lbsContent = txt }

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

-- | Increase the indentation level of a document by some amount at
-- the next newline.
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

-- | Set the indentation level to the current column.
align :: Doc -> Doc
align inner = Nest { nestDelay = True, nestAlign = True,
                     nestLevel = 0, nestDoc = inner }

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
punctuate :: Doc -> [Doc] -> Doc
punctuate punc = concat . intersperse punc

-- | Enclose a list of 'Doc's, separated by punctuation, and align
-- nesting of the contents to the end of the left enclosing 'Doc'
encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep left right _ [] = left <> right
encloseSep left right _ [doc] = left <> doc <> right
encloseSep left right middle docs =
  left <> align (punctuate middle docs) <> right

list :: [Doc] -> Doc
list = group . encloseSep lbrack rbrack (comma <> line)

-- | Erase all linebreaks in a 'Doc' and replace them with either
-- spaces or nothing, depending on the kind of linebreak.
flatten :: Doc -> Doc
flatten Line { insertSpace = True } = Char { charContent = ' ' }
flatten Line { insertSpace = False } = empty
flatten Cat { catDocs = docs } = Cat { catDocs = map flatten docs }
flatten Choose { chooseOptions = docs } =
  Choose { chooseOptions = map flatten docs }
flatten n @ Nest { nestDoc = inner } = n { nestDoc = flatten inner }
flatten doc = doc

-- | A 'Doc' that 'choose's between the unmodified argument, or the
-- 'flatten'ed version of the argument.
group :: Doc -> Doc
group doc = Choose { chooseOptions = [ doc, flatten doc ] }

buildOneLine :: Doc -> Builder
buildOneLine Char { charContent = chr } = fromChar chr
buildOneLine Bytestring { bsContent = txt } = fromByteString txt
buildOneLine LazyBytestring { lbsContent = txt } = fromLazyByteString txt
buildOneLine Line { insertSpace = True } = fromChar ' '
buildOneLine Line { insertSpace = False } = mempty
buildOneLine Cat { catDocs = docs } = mconcat (map buildOneLine docs)
buildOneLine Nest { nestDoc = inner } = buildOneLine inner
buildOneLine Choose { chooseOptions = first : _ } = buildOneLine first
buildOneLine Choose {} = error "Choose with no options"
buildOneLine Graphics { graphicsDoc = inner } = buildOneLine inner

-- | Render the entire document to one line.  Good for output that
-- will be read only by a machine, where newlines are not important at all
renderOneLine :: Doc -> Lazy.ByteString
renderOneLine = toLazyByteString . buildOneLine

buildFast :: Doc -> Builder
buildFast Char { charContent = chr } = fromChar chr
buildFast Bytestring { bsContent = txt } = fromByteString txt
buildFast LazyBytestring { lbsContent = txt } = fromLazyByteString txt
buildFast Line {} = fromChar '\n'
buildFast Cat { catDocs = docs } = mconcat (map buildFast docs)
buildFast Nest { nestDoc = inner } = buildFast inner
buildFast Choose { chooseOptions = first : _ } = buildFast first
buildFast Choose {} = error "Choose with no options"
buildFast Graphics { graphicsDoc = inner } = buildFast inner

-- | Render the entire document, preserving newlines, but without any
-- indentation.  Good for output that will be read only by machine,
-- but where newlines matter.
renderFast :: Doc -> Lazy.ByteString
renderFast = toLazyByteString . buildFast

-- | Output the entire document, as rendered by renderFast to the
-- given 'Handle'.
putFast :: Handle -> Doc -> IO ()
putFast handle =
  toByteStringIO (Strict.hPut handle) . buildFast

-- | A rendering of a document.
data Render =
  Render {
    -- | The number of lines in the document.
    renderLines :: !Word,
    -- | The largest amount by which we've overrun.
    renderOverrun :: !Column,
    -- | A builder that constructs the document.
    renderBuilder :: !(Int -> Int -> Builder),
    -- | Whether or not to add indentation on the next non-empty
    -- document.
    renderIndent :: !Indent
  }

-- | Column data type.  Represents how rendered documents affect the
-- current column.
data Column =
    -- | An absolute column offset.
    Fixed { fixedOffset :: !Int }
    -- | A relative column offset.
  | Relative { relOffset :: !Int }
    -- | The greater of a relative column offset and an absolute
    -- column offset.
  | Maximum {
      maxFixed :: !Int,
      maxRelative :: !Int
    }

-- | The indent required.
data Indent =
    -- | Indent starting with the zero column.
    Full
    -- | Indent starting with the current column.
  | Partial
    -- | No indent.
  | None
    deriving Show

instance Hashable Column where
  hashWithSalt s Fixed { fixedOffset = n } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` n
  hashWithSalt s Relative { relOffset = n } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` n
  hashWithSalt s Maximum { maxFixed = fixed, maxRelative = rel } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` fixed `hashWithSalt` rel

instance Ord Column where
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
  compare Maximum { maxFixed = fixed, maxRelative = rel }
          Fixed { fixedOffset = n } =
    case compare fixed n of
      EQ -> case compare rel n of
        EQ -> GT
        out -> out
      out -> out
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
  compare Relative { relOffset = n1 } Fixed { fixedOffset = n2 } =
    case compare n1 n2 of
      EQ -> GT
      out -> out
  compare Relative { relOffset = n }
          Maximum { maxFixed = fixed, maxRelative = rel } =
    case compare n rel of
      EQ -> case compare n fixed of
        EQ -> LT
        out -> out
      out -> out
  compare Relative { relOffset = n1 } Relative { relOffset = n2 } =
    compare n1 n2

instance Eq Column where
  c1 == c2 = compare c1 c2 == EQ

-- | Given a starting column and an ending column, give a column
-- representing the combination of the two.
advance :: Column -> Column -> Column
advance _ f @ Fixed {} = f
advance Fixed { fixedOffset = start } Relative { relOffset = n } =
  Fixed { fixedOffset = start + n }
advance Fixed { fixedOffset = start }
        Maximum { maxFixed = fixed, maxRelative = rel } =
  Fixed { fixedOffset = max fixed (start + rel) }
advance Relative { relOffset = start } Relative { relOffset = n } =
  Relative { relOffset = start + n }
advance Relative { relOffset = start } m @ Maximum { maxRelative = n } =
  m { maxRelative = start + n }
advance m @ Maximum { maxRelative = rel } Relative { relOffset = n } =
  m { maxRelative = rel + n }
advance Maximum { maxFixed = fixed1, maxRelative = rel1 }
        Maximum { maxFixed = fixed2, maxRelative = rel2 } =
  Maximum { maxFixed = max fixed2 (fixed1 + rel2), maxRelative = rel1 + rel2 }

-- | Index used in the hash table.
data Offsets =
  Offsets {
    offsetUpper :: !Int,
    offsetCol :: !Column
  }
  deriving Eq

instance Hashable Offsets where
  hashWithSalt s Offsets { offsetUpper = upper, offsetCol = col } =
    s `hashWithSalt` upper `hashWithSalt` col

-- | A result.  This is split into 'Single' and 'Multi' in order to
-- optimize for the common case of a single possible rendering.
data Result =
    -- | A single possible rendering.
    Single {
      -- | The rendered document.
      singleRender :: !Render,
      -- | The first column at which this render causes an overrun.
      singleUpper :: !Int,
      -- | The current column at the end of rendering.
      singleCol :: !Column
    }
    -- | Multiple possible renderings.
  | Multi {
      -- | A multi-level map.  The first map is indexed by the column
      -- upper-bound (meaning the first column at which using any of
      -- the contents will cause an overrun).  The second map is
      -- indexed by the ending column.
      multiOptions :: !(HashMap Offsets Render)
    }

makespaces :: Int -> Builder
makespaces n = fromLazyByteString (Lazy.Char8.replicate (fromIntegral n) ' ')

bestRender :: Render -> Render -> Render
bestRender r1 @ Render { renderLines = lines1, renderOverrun = overrun1 }
           r2 @ Render { renderLines = lines2, renderOverrun = overrun2 }
  | overrun1 < overrun2 = r1
  | overrun1 > overrun2 = r2
  | otherwise = if lines1 < lines2 then r1 else r2

insertRender :: Int -> Column -> Render -> HashMap Offsets Render ->
                HashMap Offsets Render
insertRender upper col render =
  let
    offsets = Offsets { offsetUpper = upper, offsetCol = col }
  in
    HashMap.insertWith bestRender offsets render

packResult :: HashMap Offsets Render -> Result
packResult opts =
  case HashMap.toList opts of
    [(Offsets { offsetUpper = upper, offsetCol = col }, render)] ->
      Single { singleCol = col, singleUpper = upper,
               singleRender = render }
    _ -> Multi { multiOptions = opts }

bestRenderInOpts :: HashMap Offsets Render -> Render
bestRenderInOpts =
  let
    -- | Compare two Renders.  Less than means better.
    compareRenders Render { renderLines = lines1, renderOverrun = overrun1 }
                   Render { renderLines = lines2, renderOverrun = overrun2 } =
      case compare overrun1 overrun2 of
        EQ -> compare lines1 lines2
        out -> out
  in
    minimumBy compareRenders . HashMap.elems

appendOne :: (Int, Column, Render) -> (Int, Column, Render) ->
             (Int, Column, Render)
appendOne (upper1, col1, Render { renderBuilder = build1,
                                  renderLines = lines1,
                                  renderOverrun = overrun1 })
          (upper2, col2, Render { renderBuilder = build2,
                                  renderLines = lines2,
                                  renderOverrun = overrun2,
                                  renderIndent = ind }) =
  let
    (newupper, newbuild) = case col1 of
      Fixed { fixedOffset = n } ->
        (upper1, \nesting col -> build1 nesting col `mappend` build2 nesting n)
      Relative { relOffset = n } ->
        (min upper1 (upper2 - n),
         \nesting col -> build1 nesting col `mappend`
                         (build2 nesting $! col + n))
      Maximum { maxRelative = rel, maxFixed = fixed } ->
        (min upper1 (upper2 - rel),
         \nesting col -> build1 nesting col `mappend`
                         build2 nesting (max fixed (col + rel)))

    newoverrun =
      if newupper < 0
        then Relative { relOffset = abs newupper }
        else Fixed { fixedOffset = 0 }

  in
    (newupper, col1 `advance` col2,
     Render { renderBuilder = newbuild, renderIndent = ind,
              renderOverrun = max (max overrun1 overrun2) newoverrun,
              renderLines = lines1 + lines2 })

-- | Combine two results into an option
mergeResults :: Result -> Result -> Result
mergeResults s1 @ Single { singleRender = r1 @ Render { renderLines = lines1 },
                           singleUpper = upper1,
                           singleCol = col1 }
             s2 @ Single { singleRender = r2 @ Render { renderLines = lines2 },
                           singleUpper = upper2,
                           singleCol = col2 }
  | upper1 == upper2 && col1 == col2 = if lines1 < lines2 then s1 else s2
  | otherwise =
    Multi { multiOptions = HashMap.fromList [(Offsets { offsetUpper = upper1,
                                                        offsetCol = col1 },
                                              r1),
                                             (Offsets { offsetUpper = upper2,
                                                        offsetCol = col2 },
                                              r2)] }
mergeResults Single { singleRender = render, singleUpper = upper,
                      singleCol = col }
             Multi { multiOptions = opts } =
  let
    offsets = Offsets { offsetUpper = upper, offsetCol = col }
  in
   Multi { multiOptions = HashMap.insertWith bestRender offsets render opts }
mergeResults m @ Multi {} s @ Single {} = mergeResults s m
mergeResults Multi { multiOptions = opts1 } Multi { multiOptions = opts2 } =
  Multi { multiOptions = HashMap.unionWith bestRender opts1 opts2 }

contentBuilder :: Indent -> Builder -> Int -> Int -> Builder
contentBuilder Full builder nesting _ =
  makespaces nesting `mappend` builder
contentBuilder Partial builder nesting col =
  if col < nesting
    then makespaces (nesting - col) `mappend` builder
    else builder
contentBuilder None builder _ _ = builder

buildOptimal :: Int
             -- ^ The maximum number of columns.
             -> Bool
             -- ^ Whether or not to render with ANSI terminal options.
             -> Doc
             -- ^ The document to render.
             -> Builder
buildOptimal maxcol ansiterm doc =
  let
    buildDynamic :: Graphics -> Column -> Indent -> Doc -> Result
    -- For char, bytestring, and lazy bytestring,
    buildDynamic _ _ ind Char { charContent = chr } =
      let
        overrun = if maxcol >= 1 then Relative 0 else Relative (maxcol - 1)
        builder = contentBuilder ind (fromChar chr)
      in
        Single {
          singleRender =
             Render { renderLines = 0, renderOverrun = overrun,
                      renderBuilder = builder, renderIndent = None },
          singleCol = Relative 1,
          singleUpper = maxcol - 1
        }
    buildDynamic _ _ ind Bytestring { bsContent = txt, bsLength = len } =
      let
        overrun = if maxcol >= len then Relative 0 else Relative (len - maxcol)
        builder = contentBuilder ind (fromByteString txt)
      in
       Single {
         singleRender =
             Render { renderLines = 0, renderOverrun = overrun,
                      renderBuilder = builder, renderIndent = None },
         singleCol = Relative len,
         singleUpper = maxcol - len
       }
    buildDynamic _ _ ind LazyBytestring { lbsContent = txt, lbsLength = len } =
      let
        overrun = if maxcol >= len then Relative 0 else Relative (len - maxcol)
        builder = contentBuilder ind (fromLazyByteString txt)
      in
        Single {
          singleRender =
             Render { renderLines = 0, renderOverrun = overrun,
                      renderBuilder = builder, renderIndent = None },
          singleCol = Relative len,
          singleUpper = maxcol - len
        }
    buildDynamic _ nesting _ Line {} =
      Single {
        singleRender =
           Render { renderOverrun = Fixed { fixedOffset = 0 },
                    renderIndent = Full, renderLines = 1,
                    renderBuilder = const $! const $! fromChar '\n' },
        singleCol = nesting,
        singleUpper = maxcol
      }
    buildDynamic _ _ ind Cat { catDocs = [] } =
      Single {
        singleRender =
           Render { renderOverrun = Fixed { fixedOffset = 0 },
                    renderIndent = ind, renderLines = 0,
                    renderBuilder = const mempty },
        singleCol = Relative { relOffset = 0 },
        singleUpper = maxcol
      }
    buildDynamic sgr nesting ind Cat { catDocs = first : rest } =
      let
        appendResults :: Result -> Doc -> Result
        appendResults Single { singleRender =
                                  render1 @ Render { renderIndent = ind' },
                               singleUpper = upper1, singleCol = col1 } doc' =
          case buildDynamic sgr nesting ind' doc' of
            Single { singleUpper = upper2, singleCol = col2,
                     singleRender = render2 } ->
              let
                (newupper, newcol, newrender) =
                  appendOne (upper1, col1, render1) (upper2, col2, render2)
              in
                Single { singleUpper = newupper, singleCol = newcol,
                         singleRender = newrender }
            Multi { multiOptions = opts } ->
              let
                foldfun :: HashMap Offsets Render -> Offsets -> Render ->
                           HashMap Offsets Render
                foldfun accum Offsets { offsetUpper = upper2,
                                        offsetCol = col2 } render2 =
                  let
                    (newupper, newcol, newrender) =
                      appendOne (upper1, col1, render1) (upper2, col2, render2)
                  in
                    insertRender newupper newcol newrender accum
              in
                packResult (HashMap.foldlWithKey' foldfun HashMap.empty opts)
        appendResults Multi { multiOptions = opts } doc' =
          let
            outerfold :: HashMap Offsets Render -> Offsets -> Render ->
                         HashMap Offsets Render
            outerfold accum Offsets { offsetUpper = upper1,
                                      offsetCol = col1 }
                      render1 @ Render { renderIndent = ind' } =
              case buildDynamic sgr nesting ind' doc' of
                Single { singleUpper = upper2, singleCol = col2,
                         singleRender = render2 } ->
                  let
                    (newupper, newcol, newrender) =
                      appendOne (upper1, col1, render1) (upper2, col2, render2)
                  in
                    insertRender newupper newcol newrender accum
                Multi { multiOptions = opts2 } ->
                  let
                    innerfold :: HashMap Offsets Render -> Offsets -> Render ->
                                 HashMap Offsets Render
                    innerfold accum' Offsets { offsetUpper = upper2,
                                               offsetCol = col2 } render2 =
                      let
                        (newupper, newcol, newrender) =
                          appendOne (upper1, col1, render1)
                                    (upper2, col2, render2)
                      in
                        insertRender newupper newcol newrender accum'
                  in
                    HashMap.foldlWithKey' innerfold accum opts2
          in
            packResult (HashMap.foldlWithKey' outerfold HashMap.empty opts)

        firstres = buildDynamic sgr nesting ind first
      in
        foldl appendResults firstres rest
    buildDynamic sgr nesting ind Nest { nestDelay = delay, nestDoc = inner,
                                        nestAlign = alignnest,
                                        nestLevel = lvl } =
      let
        updateRender =
          if alignnest
            then \r @ Render { renderBuilder = builder } ->
                   r { renderBuilder = \_ c -> builder (c + lvl) c }
            else \r @ Render { renderBuilder = builder } ->
                   r { renderBuilder = \n c -> builder (n + lvl) c }

        newindent = if delay then ind else Partial

        res =
          if alignnest
            then buildDynamic sgr (Relative lvl) newindent inner
            else
              let
                newnesting = case nesting of
                  Fixed { fixedOffset = n } -> Fixed { fixedOffset = n + lvl }
                  Relative { relOffset = n } -> Relative { relOffset = n + lvl }
                  Maximum { maxFixed = fixed, maxRelative = rel } ->
                    Maximum { maxFixed = fixed + lvl, maxRelative = rel + lvl }
              in
                buildDynamic sgr newnesting newindent inner
      in case res of
        s @ Single { singleRender = r } -> s { singleRender = updateRender r }
        m @ Multi { multiOptions = opts } ->
          m { multiOptions = HashMap.map updateRender opts }
    buildDynamic sgr nesting ind Choose { chooseOptions = options } =
      let
        results = map (buildDynamic sgr nesting ind) options
      in
        foldl1 mergeResults results
    buildDynamic sgr1 nesting ind Graphics { graphicsSGR = sgr2,
                                             graphicsDoc = inner }
      | ansiterm =
        let
          -- Insert graphics control characters without updating
          -- column numbers.
          wrapBuilder r @ Render { renderBuilder = build } =
            r { renderBuilder = \n c -> switchGraphics sgr1 sgr2 `mappend`
                                        build n c `mappend`
                                        switchGraphics sgr2 sgr1 }
        in case buildDynamic sgr2 nesting ind inner of
          s @ Single { singleRender = render } ->
            s { singleRender = wrapBuilder render }
          m @ Multi { multiOptions = opts } ->
            m { multiOptions = HashMap.map wrapBuilder opts }
      | otherwise = buildDynamic sgr2 nesting ind inner

    Render { renderBuilder = result } =
      case buildDynamic Default Fixed { fixedOffset = 0 } None doc of
        Single { singleRender = render } -> render
        Multi opts -> bestRenderInOpts opts
  in
    result 0 0

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
