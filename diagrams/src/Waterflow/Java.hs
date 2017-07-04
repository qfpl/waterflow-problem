{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Waterflow.Java (
    javaDiagrams
  ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Graphics.SVGFonts

import Waterflow.Common

data Vars =
  Vars {
    vLeft :: Int
  , vRight :: Int
  , vLeftMax :: Int
  , vRightMax :: Int
  , vVolume :: Int
  }

txt ::
  Colour Double ->
  String ->
  Diagram B
txt c s =
  textSVG_ def s # fc c # lc c

drawCode ::
  Maybe Int ->
  Diagram B
drawCode ix =
  let
    t = txt gray
    heights = txt black "heights"
    left = txt blue "left"
    right = txt red "right"
    leftMax = txt darkblue "leftMax"
    rightMax = txt darkred "rightMax"
    volume = txt lightblue "volume"
    lines = [
        t "int " ||| left ||| t " = 0;"
      , t "int " ||| right ||| t " = heights.length - 1;"
      , t "int " ||| leftMax ||| t " = 0;"
      , t "int " ||| rightMax ||| t " = 0;"
      , t "int " ||| volume ||| t " = 0;"
      , t " "
      , t "while(" ||| left ||| t " < " ||| right ||| t ") {"
      , t "  if(" ||| heights ||| t "[" ||| left ||| t "] > " ||| leftMax ||| t ") {"
      , t "    " ||| leftMax ||| t " = " ||| heights ||| t "[" ||| left ||| t "];"
      , t "  }"
      , t "  if(" ||| heights ||| t "[" ||| right ||| t "] > " ||| rightMax ||| t ") {"
      , t "    " ||| rightMax ||| t " = " ||| heights ||| t "[" ||| right ||| t "];"
      , t "  }"
      , t "  if(" ||| leftMax ||| t " >= " ||| rightMax ||| t ") {"
      , t "    " ||| volume ||| t " += " ||| rightMax ||| t " - " ||| heights ||| t "[" ||| right ||| t "];"
      , t "    " ||| right ||| t "--;"
      , t "  } else {"
      , t "    " ||| volume ||| t " += " ||| leftMax ||| t " - " ||| heights ||| t "[" ||| left ||| t "];"
      , t "    " ||| left ||| t "--;"
      , t "  }"
      , t "}"
      , t "return " ||| volume ||| t ";"
      ]
  in
    vcat lines

javaDiagrams ::
  Problem ->
  [Diagram B]
javaDiagrams _ =
  [drawCode Nothing]
