{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Waterflow where

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

drawListFn ::
  ([Int] -> [Int]) ->
  String ->
  Problem ->
  Diagram B
drawListFn g label (Problem hs) =
  let
    f h = (textSVG_ def (show h)) # fc black # centerX <> square 1 # fc white # lc white # lw veryThick
  in
    hcat (map f . g $ hs) |||
    strutX space |||
    textSVG_ def label # fc black

drawHeightList ::
  Problem ->
  Diagram B
drawHeightList =
  drawListFn id "heights"

drawProblemAndHeights ::
  Problem ->
  Diagram B
drawProblemAndHeights p =
  vsep space . fmap ($ p) $ [
      drawProblem
    , drawHeightList
    ]

-- animate a foldl1 max of the heights
-- - weave the lines through on the graph

drawFold1Max ::
  Problem ->
  Int ->
  Diagram B
drawFold1Max p@(Problem hs) i =
  let
    col x
      | x == i || (i == length hs && x == i - 1) = black
      | x == i - 1 = gray
      | otherwise = white
    f x h = (textSVG_ def (show h)) # fc (col x) # lc (col x)# centerX <> square 1 # fc white # lc white # lw veryThick
    s = scanl1 max hs
  in
    hcat (zipWith f [0..] s) |||
    strutX space |||
    textSVG_ def "fold1 max" # fc black

--    o = fromOffsets . map r2 $ [(0, 0), (1, 0), (0, 3), (1, 0), (0, -4)]
--    l = o # strokeLine # lc blue # lw veryThick # translate (r2 (-0.5, -5.5))

drawFold1MaxLine ::
  Problem ->
  Int ->
  Diagram B
drawFold1MaxLine p@(Problem hs) i =
  let
    gridHeight = 1 + maximum hs
    across = r2 (1.0, 00)
    s = take (i + 1) . scanl1 max $ hs
    s' = case s of
      [] -> []
      (h : t) -> h : s
    o = fromOffsets .
        (++ [across]) .
        intersperse across .
        fmap (\x -> r2 (0.0, fromIntegral x)) .
        zipWith (-) s $
        s'
  in
    case s of
      [] -> mempty
      (h : _) -> o # strokeLine # lc blue # lw veryThick # translate (r2 (-0.5, 0.5 + (fromIntegral $ h - gridHeight)))

drawProblemAndFoldl1Max' ::
  Problem ->
  Int ->
  Diagram B
drawProblemAndFoldl1Max' p i =
  vsep space [
      drawFold1MaxLine p i `atop` drawProblem p
    , drawHeightList p
    , drawFold1Max p i
    ]

drawProblemAndFoldl1Max ::
  Problem ->
  [Diagram B]
drawProblemAndFoldl1Max p =
  fmap (drawProblemAndFoldl1Max' p) [0 .. length (heights p)]

drawProblemAndScanl1Max ::
  Problem ->
  Diagram B
drawProblemAndScanl1Max p =
  vsep space [
      drawFold1MaxLine p (length . heights $ p) `atop` drawProblem p
    , drawHeightList p
    , drawListFn (scanl1 max) "scanl1 max heights" p
    ]

diagrams ::
  Problem ->
  [Diagram B]
diagrams p =
  fmap ($ p) [drawProblem, drawSolution, drawProblemAndHeights] ++
  drawProblemAndFoldl1Max p ++
  [drawProblemAndScanl1Max p]

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

