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

module Text.Format.Doc(
       -- * Basic Definitions
       -- ** Types
       Doc(..),
       Graphics(..),
       LineKind(..),

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
       ) where

import Data.Hashable
import Data.List(intersperse, sort)
import Data.Maybe
import Prelude hiding ((<$>), concat)
import System.Console.ANSI
import Text.Format.Graphics

import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict.UTF8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.UTF8 as Lazy.UTF8

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
