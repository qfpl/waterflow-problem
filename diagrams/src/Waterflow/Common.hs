{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Waterflow.Common where

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
