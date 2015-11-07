-- Copyright (c) 2014 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
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

module Tests.Text.Format(tests) where

import Test.HUnitPlus.Base
import Text.Format

import qualified Data.ByteString.UTF8 as Strict
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.ByteString.Lazy.UTF8 as Lazy

renderOptimalTests :: [Test]
renderOptimalTests =
  let
    innerstructdoc = string "inner(" <> align (string "123" </>
                                                string "456") <//>
                                         rparen
    structdoc = string "pre (" <> align (string "hello" </>
                                         string "world" <!>
                                         string "aaaa" </>
                                         string "bbb") <//>
                                  rparen
    nesteddoc = string "pre (" <> align (innerstructdoc </>
                                         string "hello" <!>
                                         string "aaaa" </>
                                         string "bbb") <//>
                                  rparen
    nesteddoc2 = string "pre (" <> align (string "hello" </>
                                          innerstructdoc <!>
                                          string "aaaa" </>
                                          string "bbb") <//>
                                   rparen
  in [
    "empty" ~: Lazy.empty @=? renderOptimal 1 False empty,
    "char" ~: Lazy.singleton 'a' @=? renderOptimal 1 False (char 'a'),
    "string" ~: Lazy.fromString "hello" @=?
      renderOptimal 1 False (string "hello"),
    "bytestring" ~: Lazy.fromString "hello" @=?
      renderOptimal 1 False (bytestring (Strict.fromString "hello")),
    "lazyBytestring" ~: Lazy.fromString "hello" @=?
      renderOptimal 1 False (lazyBytestring (Lazy.fromString "hello")),
    "line" ~: Lazy.singleton '\n' @?= renderOptimal 1 False line,
    "cat" ~: Lazy.fromString "helloworld" @=?
      renderOptimal 1 False (string "hello" <> string "world"),
    "nest" ~: Lazy.fromString "hello\n  world" @=?
      renderOptimal 1 False (string "hello" <> nest 2 (line <> string "world")),
    "align" ~: Lazy.fromString "hello\n     world" @=?
      renderOptimal 1 False (string "hello" <> align (line <> string "world")),
    "align2" ~: Lazy.fromString "hello\n     world\n     today" @=?
      renderOptimal 1 False (string "hello" <>
                             align (line <> string "world" <>
                                    line <> string "today")),
    "align3" ~: Lazy.fromString "hello\n     world\n          today" @=?
      renderOptimal 1 False (string "hello" <>
                             align (line <> string "world" <>
                                    align (line <> string "today"))),
    "align_text_break" ~: Lazy.fromString "hello world\n     today" @=?
      renderOptimal 1 False (string "hello" <>
                             align (char ' ' <> string "world" <!>
                                    string "today")),
    "align_text_break_nest" ~: Lazy.fromString "hello world\n       today" @=?
      renderOptimal 1 False (string "hello" <>
                             align (char ' ' <> string "world" <!>
                                    nest 2 (string "today"))),
    "indent" ~: Lazy.fromString "  hello" @=?
      renderOptimal 1 False (indent 2 (string "hello")),
    "indent_line" ~: Lazy.fromString "  hello\n  world" @=?
      renderOptimal 1 False (indent 2 (string "hello" <> line <>
                                       string "world")),
    "indent_nop" ~: Lazy.fromString "hello world" @=?
      renderOptimal 1 False (string "hello" <+> indent 2 (string "world")),
    "indent_immediate" ~: Lazy.fromString "hello     world" @=?
      renderOptimal 1 False (string "hello" <> indent 10 (string "world")),
    "indent_hang" ~: Lazy.fromString "hello world\n  today" @=?
      renderOptimal 1 False (string "hello" <+> indent 2 (string "world" <>
                                                          line <>
                                                          string "today")),
    "choose_lines" ~: Lazy.fromString "\n" @=?
      renderOptimal 2 False (choose [line <> line <> line, line, line <> line]),
    "choose_overrun" ~: Lazy.fromString " " @=?
      renderOptimal 1 False (choose [string " ", string "  "]),
    "choose_line_cat" ~: Lazy.fromString "hello\nworld" @=?
      renderOptimal 8 False (choose [string "hello ", string "hello" <> line] <>
                       string "world"),
    "choose_nest_line_cat" ~: Lazy.fromString "hello\nworld" @=?
      renderOptimal 8 False (choose [string "hello ",
                               string "hello" <> nest 2 line] <>
                       string "world"),
    "cat_choose" ~: Lazy.fromString "hello\nworld" @=?
      renderOptimal 8 False (string "hello" <>
                             choose [string " world", line <> string "world"]),
    "cat_nest_choose" ~: Lazy.fromString "hello\n  world" @=?
      renderOptimal 8 False (string "hello" <>
                             nest 2 (choose [string " world",
                                             line <> string "world"])),
    "choose_choose" ~: Lazy.fromString "hello you" @=?
      renderOptimal 9 False (choose [string "hello ", string "hello" <> line] <>
                       choose [string "world", string "you"]),
    "softline_break" ~: Lazy.fromString "hello\nworld" @=?
      renderOptimal 6 False (string "hello" </> string "world"),
    "softline_space" ~: Lazy.fromString "hello world" @=?
      renderOptimal 11 False (string "hello" </> string "world"),
    "delay_indent" ~: Lazy.fromString "\n\n" @=?
      renderOptimal 2 False (nest 2 (line <> line)),
    "delay_indent2" ~: Lazy.fromString "\n\n  hello" @=?
      renderOptimal 2 False (nest 2 (line <> line <> string "hello")),
    "delay_indent_nest" ~: Lazy.fromString "\n\n    hello" @=?
      renderOptimal 2 False (nest 2 (line <> nest 2 (line <> string "hello"))),
    "align_delay_indent" ~: Lazy.fromString "  \n\n  hello" @=?
      renderOptimal 2 False (string "  " <>
                             align (line <> line <> string "hello")),
    "align_delay_indent_nest" ~: Lazy.fromString "  \n\n    hello" @=?
      renderOptimal 2 False (string "  " <>
                             align (nest 2 (line <> line <> string "hello"))),
    "align_delay_indent_line_nest" ~: Lazy.fromString "  \n\n    hello" @=?
      renderOptimal 2 False (string "  " <>
                             align (line <> nest 2 (line <> string "hello"))),
    "multiline_break" ~: Lazy.fromString "aaaaa\nbbbbc\ndddee\nfghij" @=?
      renderOptimal 5 False (string "aaaaa" <//> string "bbbb" <//>
                             char 'c' <//> string "ddd" <//> string "ee" <//>
                             char 'f' <//> char 'g' <//> char 'h' <//>
                             char 'i' <//> char 'j'),
    "softline_break_hardline_softline_break" ~:
      Lazy.fromString "hello\nworld\naaaa\nbbb" @=?
        renderOptimal 6 False (string "hello" </> string "world" <!>
                               string "aaaa" </> string "bbb"),
    "softline_break_hardline_softline_space" ~:
      Lazy.fromString "hello\nworld\naaaa bbb" @=?
        renderOptimal 8 False (string "hello" </> string "world" <!>
                               string "aaaa" </> string "bbb"),
    "softline_space_hardline_softline_space" ~:
      Lazy.fromString "hello world\naaaa bbb" @=?
        renderOptimal 11 False (string "hello" </> string "world" <!>
                                string "aaaa" </> string "bbb"),
    "constructor_break_break" ~:
      Lazy.fromString "pre (hello\n     world\n     aaaa\n     bbb)" @=?
      renderOptimal 10 False structdoc,
    "constructor_break_space" ~:
      Lazy.fromString "pre (hello\n     world\n     aaaa bbb)" @=?
      renderOptimal 14 False structdoc,
    "constructor_space_space" ~:
      Lazy.fromString "pre (hello world\n     aaaa bbb)" @=?
      renderOptimal 16 False structdoc,
    "constructor_nested" ~:
      Lazy.fromString "pre (inner(123\n           456)\n     hello\n     aaaa bbb)" @=?
      renderOptimal 15 False nesteddoc,
    "constructor_nested2" ~:
      Lazy.fromString "pre (hello\n     inner(123\n           456)\n     aaaa bbb)" @=?
      renderOptimal 15 False nesteddoc2,
    "dullWhite" ~: Lazy.fromString "\ESC[37mhello\ESC[0m" @=?
      renderOptimal 20 True (dullWhite (string "hello")),
    "dullWhite_left_concat" ~: Lazy.fromString "\ESC[37mhello\ESC[0mworld" @=?
      renderOptimal 20 True (dullWhite (string "hello") <> string "world"),
    "dullWhite_right_concat" ~: Lazy.fromString "hello\ESC[37mworld\ESC[0m" @=?
      renderOptimal 20 True (string "hello" <> dullWhite (string "world")),
    "dullWhite_inner_concat" ~: Lazy.fromString "\ESC[37mhelloworld\ESC[0m" @=?
      renderOptimal 20 True (dullWhite (string "hello" <> string "world"))
  ]

testlist :: [Test]
testlist = [
    "renderOptimal" ~: renderOptimalTests
  ]

tests :: Test
tests = "Format" ~: testlist
