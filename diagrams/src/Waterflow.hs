{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Waterflow where

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

drawWater ::
  Problem ->
  Diagram B
drawWater p@(Problem hs) =
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
  drawWater p `atop` drawProblem p

drawHeightList ::
  Problem ->
  Diagram B
drawHeightList (Problem hs) =
  let
    f h = (textSVG_ def (show h)) # fc black # centerX <> square 1 # fc white # lc white # lw veryThick
  in
    hcat $ map f hs

drawProblemAndHeightList ::
  Problem ->
  Diagram B
drawProblemAndHeightList p =
  drawProblem p
  ===
  strutY space
  ===
  (drawHeightList p ||| strutX space ||| textSVG_ def "Height" # fc black)

diagrams ::
  Problem ->
  [Diagram B]
diagrams p =
  fmap ($ p) [drawProblem, drawSolution, drawProblemAndHeightList]

-- animate a foldl1 max of the heights
-- - weave the lines through on the graph
-- - animate a reveal of scanl1 max at the same time
--
-- animate a foldr1 max of the heights
-- - weave the lines through on the graph
-- - animate a reveal of scanr1 max at the same time
--
-- show scanl max and scanr max
-- - include both lines on the graph
--
-- animate zipWith min maxL maxR
-- - on the numbers and the lines simultaneously
--
-- animate zipWith (-) heights
-- - animate the numbers while bringing in the volume blocks
--
-- add sum volume
-- - merge the volume blocks
--
-- optional: show that we can inline the code

