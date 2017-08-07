{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Waterflow.Java (
    javaDiagrams
  ) where

import Control.Monad (when)

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer hiding ((<>))

import Control.Lens hiding ((#), beside)

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Graphics.SVGFonts

import Waterflow.Common

import Java.Program
import Java.Model.Expr
import Java.Model.Stm

data WaterflowVars =
  WaterflowVars {
    _wHeights :: [Int]
  , _wLeft :: Int
  , _wRight :: Int
  , _wLeftMax :: Int
  , _wRightMax :: Int
  , _wVolume :: Int
  } deriving (Eq, Show)

makeLenses ''WaterflowVars

vHeights :: Var WaterflowVars [Int]
vHeights = Var "heights" black "int[]" wHeights

vLeft :: Var WaterflowVars Int
vLeft = Var "left" blue "int" wLeft

vRight :: Var WaterflowVars Int
vRight = Var "right" red "int" wRight

vLeftMax :: Var WaterflowVars Int
vLeftMax = Var "leftMax" darkblue "int" wLeftMax

vRightMax :: Var WaterflowVars Int
vRightMax = Var "rightMax" darkred "int" wRightMax

vVolume :: Var WaterflowVars Int
vVolume = Var "volume" lightblue "int" wVolume

waterflow :: Problem -> Stm WaterflowVars Int
waterflow (Problem hs) =
  let
    heightsIx v =
      ReadRef (RIx (RVar vHeights) (ReadRef (RVar v)))
  in
    DeclVar vHeights (Lit hs) `Seq`
    DeclVar vLeft (Lit 0) `Seq`
    DeclVar vRight (Sub (Length (ReadRef (RVar vHeights))) (Lit 1)) `Seq`
    DeclVar vLeftMax (Lit 0) `Seq`
    DeclVar vRightMax (Lit 0) `Seq`
    DeclVar vVolume (Lit 0) `Seq`
    While (Lt (ReadRef (RVar vLeft)) (ReadRef (RVar vRight))) (
      If (Gt (heightsIx vLeft) (ReadRef (RVar vLeftMax))) (
          WriteRef (RVar vLeftMax) (ReadRef (RIx (RVar vHeights) (ReadRef (RVar vLeft))))
      ) `Seq`
      If (Gt (heightsIx vRight) (ReadRef (RVar vRightMax))) (
          WriteRef (RVar vRightMax) (ReadRef (RIx (RVar vHeights) (ReadRef (RVar vRight))))
      ) `Seq`
      IfElse (Gte (ReadRef (RVar vLeftMax)) (ReadRef (RVar vRightMax))) (
          WriteAdd (RVar vVolume) (Sub (ReadRef (RVar vRightMax)) (heightsIx vRight)) `Seq`
          PostDec (RVar vRight)
      ) (
          WriteAdd (RVar vVolume) (Sub (ReadRef (RVar vLeftMax)) (heightsIx vLeft)) `Seq`
          PostInc (RVar vLeft)
      )
    ) `Seq`
    Return (ReadRef (RVar vVolume))

drawLeftAndRight ::
  Int ->
  Int ->
  Int ->
  Diagram B
drawLeftAndRight h l r =
  let
    f x
      | x == l && x == r = txt blue "L" <> txt red "R"
      | x == l = txt blue "L"
      | x == r = txt red "R"
      | otherwise = mempty
    row = hcat . fmap (onSquare . f) $ [0 .. h-1]
  in
    row

drawVolume ::
  Int ->
  Int ->
  Diagram B
drawVolume h v =
  let
    f x
      | x == (h `div` 2) = txtShow lightblue v
      | otherwise = mempty
    row = hcat . fmap (onSquare . f) $ [0 .. h-1]
  in
    hsep space [row, txt lightblue "volume"]

labelLeftMax ::
  Int ->
  Int ->
  Diagram B
labelLeftMax mh lm =
  let
    gridHeight = 1 + mh
    yOffset = 0.5 + (-1 * fromIntegral (gridHeight - lm))
  in
    txt darkblue "LM" # centerY # translateY yOffset

labelRightMax ::
  Int ->
  Int ->
  Diagram B
labelRightMax mh rm =
  let
    gridHeight = 1 + mh
    yOffset = 0.5 + (-1 * fromIntegral (gridHeight - rm))
  in
    txt darkred "RM" # centerY # translateY yOffset

drawVLine ::
  Int ->
  Int ->
  Colour Double ->
  Diagram B
drawVLine h x col =
  let
    path = fromOffsets [r2 (0, -1 * fromIntegral h)]
  in
    path # strokeLine
         # fc col
         # lc col
         # translateX (fromIntegral x)
         # translateY 0.5
         # dashingN [0.005, 0.005] 0

drawHLine ::
  Int ->
  Int ->
  Colour Double ->
  Diagram B
drawHLine w y col =
  let
    path = fromOffsets [r2 (fromIntegral w, 0)]
  in
    path # strokeLine
         # fc col
         # lc col
         # translateY (0.5 + (-1) * fromIntegral y)
         # translateX (-0.5)
         # dashingN [0.005, 0.005] 0

drawCircle ::
  Int ->
  Int ->
  Colour Double ->
  Diagram B
drawCircle x y col =
  circle 0.08 # fc col # lc col
              # translateX (fromIntegral x)
              # translateY (0.5 + (-1) * fromIntegral y)

drawProblem' ::
  WaterflowVars ->
  Diagram B
drawProblem' (WaterflowVars hs l r lm rm _) =
  let
    gridHeight = 1 + maximum hs
    gridWidth = length hs
    mh = maximum hs
    lLeft = labelLeftMax mh lm
    lRight = labelRightMax mh rm
    problemWithLines = mconcat [
        drawCircle l (gridHeight - hs !! l) blue
      , drawVLine gridHeight l blue
      , drawHLine gridWidth (gridHeight - lm) darkblue
      , drawCircle r (gridHeight - hs !! r) red
      , drawVLine gridHeight r red
      , drawHLine gridWidth (gridHeight - rm) darkred
      , drawProblem (Problem hs)
      ]
  in
    beside (r2 (1, 0)) (beside (r2 (-1, 0)) problemWithLines (lLeft ||| strutX space)) (strutX space ||| lRight)

drawVars :: WaterflowVars -> Diagram B
drawVars v@(WaterflowVars heights left right leftMax rightMax volume) =
  let
    h = length heights
    pieces = [
        drawProblem' v
      , drawLeftAndRight h left right
      , drawVolume h volume
      ]
  in
    vsep space pieces

javaDiagrams :: Problem -> [Diagram B]
javaDiagrams p@(Problem hs) =
  animateProgram (WaterflowVars hs 0 0 0 0 0) (runStm $ waterflow p) drawVars
