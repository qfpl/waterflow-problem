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

txt ::
  Colour Double ->
  String ->
  Diagram B
txt c s =
  textSVG_ def s # fc c # lc c

t ::
  String ->
  Diagram B
t = txt gray

tHeights, tLeft, tRight, tLeftMax, tRightMax, tVolume :: Diagram B
tHeights = txt black "heights"
tLeft = txt blue "left"
tRight = txt red "right"
tLeftMax = txt darkblue "leftMax"
tRightMax = txt darkred "rightMax"
tVolume = txt lightblue "volume"

data Ix =
    Row Int
  | Segment Int Int

drawCode ::
  [(Ix, Colour Double)] ->
  Diagram B
drawCode ix =
  let
    -- TODO break up the segments more
    segments = [
        pure $ t "int " ||| tLeft ||| t " = 0;"
      , pure $ t "int " ||| tRight ||| t " = tHeights.length - 1;"
      , pure $ t "int " ||| tLeftMax ||| t " = 0;"
      , pure $ t "int " ||| tRightMax ||| t " = 0;"
      , pure $ t "int " ||| tVolume ||| t " = 0;"
      , pure $ t " "
      , pure $ t "while(" ||| tLeft ||| t " < " ||| tRight ||| t ") {"
      , pure $ t "  if(" ||| tHeights ||| t "[" ||| tLeft ||| t "] > " ||| tLeftMax ||| t ") {"
      , pure $ t "    " ||| tLeftMax ||| t " = " ||| tHeights ||| t "[" ||| tLeft ||| t "];"
      , pure $ t "  }"
      , pure $ t "  if(" ||| tHeights ||| t "[" ||| tRight ||| t "] > " ||| tRightMax ||| t ") {"
      , pure $ t "    " ||| tRightMax ||| t " = " ||| tHeights ||| t "[" ||| tRight ||| t "];"
      , pure $ t "  }"
      , pure $ t "  if(" ||| tLeftMax ||| t " >= " ||| tRightMax ||| t ") {"
      , [t "    " , tVolume , t " += ", tRightMax ||| t " - " ||| tHeights ||| t "[" ||| tRight ||| t "]", t ";"]
      , pure $ t "    " ||| tRight ||| t "--;"
      , pure $ t "  } else {"
      , [t "    " , tVolume , t " += " , tLeftMax ||| t " - " ||| tHeights ||| t "[" ||| tLeft ||| t "]", t ";"]
      , pure $ t "    " ||| tLeft ||| t "--;"
      , pure $ t "  }"
      , pure $ t "}"
      , pure $ t "return " ||| tVolume ||| t ";"
      ]
  in
    vcat . fmap hcat $ segments

data Vars =
  Vars {
    vLeft :: Int
  , vRight :: Int
  , vLeftMax :: Int
  , vRightMax :: Int
  , vVolume :: Int
  }

initialVars ::
  Problem ->
  Vars
initialVars p =
  Vars 0 (pred . length . heights $ p) 0 0 0

annotateProblem ::
  Problem ->
  Vars ->
  Diagram B
annotateProblem p v =
  -- TODO still need to draw the variables and the various lines
  drawProblem p

javaDiagrams ::
  Problem ->
  [Diagram B]
javaDiagrams p =
  -- TODO match the height of these two
  [ drawCode [] ||| annotateProblem p (initialVars p)
  ]
