{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Waterflow.Common where

import Data.List (intersperse)

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Graphics.SVGFonts

newtype Problem = Problem { heights :: [Int] }
  deriving (Eq, Show)

sampleProblem ::
  Problem
sampleProblem =
  Problem [2, 5, 1, 2, 3, 4, 7, 7, 6]

waterHeights ::
  Problem ->
  [Int]
waterHeights (Problem h) =
  zipWith min
    (scanl1 max h)
    (scanr1 max h)

solve ::
  Problem ->
  Int
solve p@(Problem h) =
  sum (zipWith (-)
        (waterHeights p)
        h)

gridBlock :: Diagram B
gridBlock =
  square 1.0 # lc white # lw veryThick

building :: Diagram B
building =
  gridBlock # fc darkgray

sky :: Diagram B
sky =
  gridBlock # fc lightgray

water :: Diagram B
water =
  gridBlock # fc lightblue

space :: Double
space = 0.1

txt ::
  Colour Double ->
  String ->
  Diagram B
txt c s =
  textSVG_ waterflowTextOpts s # fc c # lc c # lwN 0.001

waterflowTextOpts ::
  TextOpts Double
waterflowTextOpts =
  TextOpts
    lin
    INSIDE_H
    KERN
    False
    1
    1

txtShow ::
  Show a =>
  Colour Double ->
  a ->
  Diagram B
txtShow c =
  txt c .
  show

t ::
  String ->
  Diagram B
t =
  txt gray

txtBg :: Diagram B
txtBg = square 1 # fc white # lc white # lw veryThick

onSquare ::
  Diagram B ->
  Diagram B
onSquare d =
  d # centerX <> txtBg

drawListFn ::
  ([Int] -> [Int]) ->
  Colour Double ->
  Diagram B ->
  Problem ->
  Diagram B
drawListFn g col label (Problem hs) =
  let
    f = onSquare . txtShow col
  in
    hcat (map f . g $ hs) |||
    strutX space |||
    label

drawZipWith' ::
  [Int] ->
  Colour Double ->
  Diagram B ->
  Int ->
  Diagram B
drawZipWith' vs c label i =
  let
    col x
      | (x <= i) = c
      | otherwise = white
    f x = onSquare . txtShow (col x)
  in
    hcat (zipWith f [0..] vs) |||
    strutX space |||
    label

drawZipWithLine' ::
  Problem ->
  [Int] ->
  Colour Double ->
  Int ->
  Diagram B
drawZipWithLine' (Problem hs) vs col i =
  let
    gridHeight = 1 + maximum hs
    across = r2 (1.0, 00)
    s = take (i + 1) vs
    s' = case s of
      [] -> []
      (h : _) -> h : s
    o = fromOffsets .
        (++ [across]) .
        intersperse across .
        fmap (\x -> r2 (0.0, fromIntegral x)) .
        zipWith (-) s $
        s'
  in
    case s of
      [] -> mempty
      (h : _) -> o # strokeLine # lc col # lw veryThick # translate (r2 (-0.5, 0.5 + (fromIntegral $ h - gridHeight)))

drawHeightList ::
  Problem ->
  Diagram B
drawHeightList =
  drawListFn id black $ txt black "heights"

drawProblem ::
  Problem ->
  Diagram B
drawProblem (Problem hs) =
  let
    gridHeight = 1 + maximum hs
    col h = vcat $ replicate (gridHeight - h) sky ++ replicate h building
    d = hcat $ fmap col hs
  in
    d

drawWaterSquares ::
  Problem ->
  Diagram B
drawWaterSquares p@(Problem hs) =
  let
    gridHeight = 1 + maximum hs
    ws = waterHeights p
    col w h =
      if w == h
      then strutX 1
      else vcat $ replicate (gridHeight - w) (strutY 1) ++ replicate (w - h) water ++ replicate h (strutY 1)
    d = hcat $ zipWith col ws hs
  in
    d

drawSolution ::
  Problem ->
  Diagram B
drawSolution p =
  drawWaterSquares p `atop` drawProblem p

commonDiagrams ::
  Problem ->
  [Diagram B]
commonDiagrams p =
  [ drawProblem p
  , drawSolution p
  ]
