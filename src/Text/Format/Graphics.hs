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

module Text.Format.Graphics(
       Graphics(..),
       switchGraphics
       ) where

import Control.Arrow((***))
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.Hashable
import System.Console.ANSI

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

-- | Generate a 'Builder' representing a graphics mode switch.
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
