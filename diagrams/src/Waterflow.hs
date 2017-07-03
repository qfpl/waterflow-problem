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
  gridBlock # fc blue

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

drawFoldl1Max ::
  Problem ->
  Int ->
  Diagram B
drawFoldl1Max p@(Problem hs) i =
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
    textSVG_ def "foldl1 max" # fc black

drawFoldl1MaxLine ::
  Problem ->
  Int ->
  Diagram B
drawFoldl1MaxLine p@(Problem hs) i =
  let
    gridHeight = 1 + maximum hs
    across = r2 (1.0, 00)
    s = take (i + 1) . scanl1 max $ hs
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
      (h : _) -> o # strokeLine # lc blue # lw veryThick # translate (r2 (-0.5, 0.5 + (fromIntegral $ h - gridHeight)))

drawProblemAndFoldl1Max' ::
  Problem ->
  Int ->
  Diagram B
drawProblemAndFoldl1Max' p i =
  vsep space [
      drawFoldl1MaxLine p i `atop` drawProblem p
    , drawHeightList p
    , drawFoldl1Max p i
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
      drawFoldl1MaxLine p (length . heights $ p) `atop` drawProblem p
    , drawHeightList p
    , drawListFn (scanl1 max) "maxL = scanl1 max heights" p
    ]

drawFoldr1Max ::
  Problem ->
  Int ->
  Diagram B
drawFoldr1Max p@(Problem hs) i =
  let
    col x
      | x == i || (i == length hs && x == i - 1) = black
      | x == i - 1 = gray
      | otherwise = white
    f x h = (textSVG_ def (show h)) # fc (col x) # lc (col x)# centerX <> square 1 # fc white # lc white # lw veryThick
    s = scanr1 max hs
  in
    hcat (reverse . zipWith f [0..] . reverse $ s) |||
    strutX space |||
    textSVG_ def "foldr1 max" # fc black

drawFoldr1MaxLine ::
  Problem ->
  Int ->
  Diagram B
drawFoldr1MaxLine p@(Problem hs) i =
  let
    gridHeight = 1 + maximum hs
    gridWidth = length hs
    across = r2 (-1.0, 00)
    s = take (i + 1) . reverse . scanr1 max $ hs
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
      (h : _) -> o # strokeLine # lc red # lw veryThick # translate (r2 (-0.5 + (fromIntegral gridWidth), 0.5 + (fromIntegral $ h - gridHeight)))

drawProblemAndFoldr1Max' ::
  Problem ->
  Int ->
  Diagram B
drawProblemAndFoldr1Max' p i =
  vsep space [
      drawFoldr1MaxLine p i `atop` drawFoldl1MaxLine p (length . heights $ p) `atop` drawProblem p
    , drawHeightList p
    , drawListFn (scanl1 max) "maxL = scanl1 max heights" p
    , drawFoldr1Max p i
    ]

drawProblemAndFoldr1Max ::
  Problem ->
  [Diagram B]
drawProblemAndFoldr1Max p =
  fmap (drawProblemAndFoldr1Max' p) [0 .. length (heights p)]

drawProblemAndScanr1Max ::
  Problem ->
  Diagram B
drawProblemAndScanr1Max p =
  vsep space [
      drawFoldr1MaxLine p (length . heights $ p) `atop` drawFoldl1MaxLine p (length . heights $ p) `atop` drawProblem p
    , drawHeightList p
    , drawListFn (scanl1 max) "maxL = scanl1 max heights" p
    , drawListFn (scanr1 max) "maxR = scanr1 max heights" p
    ]

diagrams ::
  Problem ->
  [Diagram B]
diagrams p =
  fmap ($ p) [drawProblem, drawSolution, drawProblemAndHeights] ++
  drawProblemAndFoldl1Max p ++
  [drawProblemAndScanl1Max p] ++
  drawProblemAndFoldr1Max p ++
  [drawProblemAndScanr1Max p]

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

