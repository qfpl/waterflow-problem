{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Waterflow where

import Data.List (intersperse, groupBy)

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

drawListFn ::
  ([Int] -> [Int]) ->
  Colour Double ->
  Diagram B ->
  Problem ->
  Diagram B
drawListFn g col label (Problem hs) =
  let
    f h = (textSVG_ def (show h)) # fc col # lc col # centerX <> square 1 # fc white # lc white # lw veryThick
  in
    hcat (map f . g $ hs) |||
    strutX space |||
    label

drawHeightList ::
  Problem ->
  Diagram B
drawHeightList =
  drawListFn id black $ textSVG_ def "heights" # fc black # lc black

drawProblemAndHeights ::
  Problem ->
  Diagram B
drawProblemAndHeights p@(Problem hs) =
  let
    h = length hs
  in
    vsep space  [
        drawZipWithLine' p hs black h `atop`
        drawProblem p
      , drawHeightList p
      ]

drawFoldl1Max ::
  Problem ->
  Int ->
  Diagram B
drawFoldl1Max p@(Problem hs) i =
  let
    col x
      | x == i || (i == length hs && x == i - 1) = blue
      | x == i - 1 = lightblue
      | otherwise = white
    f x h = (textSVG_ def (show h)) # fc (col x) # lc (col x)# centerX <> square 1 # fc white # lc white # lw veryThick
    s = scanl1 max hs
  in
    hcat (zipWith f [0..] s) |||
    strutX space |||
    textSVG_ def "foldl1 max" # fc gray # lc gray

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
drawProblemAndFoldl1Max' p@(Problem hs) i =
  let
    h = length hs
  in
    vsep space [
        drawFoldl1MaxLine p i `atop`
        drawZipWithLine' p hs black h `atop`
        drawProblem p
      , drawHeightList p
      , drawFoldl1Max p i
      ]

drawProblemAndFoldl1Max ::
  Problem ->
  [Diagram B]
drawProblemAndFoldl1Max p =
  fmap (drawProblemAndFoldl1Max' p) [0 .. length (heights p)]

maxLLabel ::
  Diagram B
maxLLabel =
  textSVG_ def "maxL" # fc blue # lc blue |||
  textSVG_ def " = scanl1 max " # fc gray # lc gray |||
  textSVG_ def "heights" # fc black # lc black

drawProblemAndScanl1Max ::
  Problem ->
  Diagram B
drawProblemAndScanl1Max p@(Problem hs) =
  let
    h = length hs
  in
    vsep space [
        drawFoldl1MaxLine p h `atop`
        drawZipWithLine' p hs black h `atop`
        drawProblem p
      , drawHeightList p
      , drawListFn (scanl1 max) blue maxLLabel p
      ]

drawFoldr1Max ::
  Problem ->
  Int ->
  Diagram B
drawFoldr1Max p@(Problem hs) i =
  let
    col x
      | x == i || (i == length hs && x == i - 1) = red
      | x == i - 1 = lightpink
      | otherwise = white
    f x h = (textSVG_ def (show h)) # fc (col x) # lc (col x)# centerX <> square 1 # fc white # lc white # lw veryThick
    s = scanr1 max hs
  in
    hcat (reverse . zipWith f [0..] . reverse $ s) |||
    strutX space |||
    textSVG_ def "foldr1 max" # fc gray # lc gray

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
drawProblemAndFoldr1Max' p@(Problem hs) i =
  let
    h = length hs
  in
    vsep space [
        drawFoldr1MaxLine p i `atop`
        drawFoldl1MaxLine p h `atop`
        drawZipWithLine' p hs black h `atop`
        drawProblem p
      , drawHeightList p
      , drawListFn (scanl1 max) blue maxLLabel p
      , drawFoldr1Max p i
      ]

drawProblemAndFoldr1Max ::
  Problem ->
  [Diagram B]
drawProblemAndFoldr1Max p =
  fmap (drawProblemAndFoldr1Max' p) [0 .. length (heights p)]

maxRLabel ::
  Diagram B
maxRLabel =
  textSVG_ def "maxR" # fc red # lc red |||
  textSVG_ def " = scanl1 max " # fc gray # lc gray |||
  textSVG_ def "heights" # fc black # lc black

drawProblemAndScanr1Max ::
  Problem ->
  Diagram B
drawProblemAndScanr1Max p@(Problem hs) =
  let
    h = length hs
  in
    vsep space [
        drawFoldr1MaxLine p h `atop`
        drawFoldl1MaxLine p h `atop`
        drawZipWithLine' p hs black h `atop`
        drawProblem p
      , drawHeightList p
      , drawListFn (scanl1 max) blue maxLLabel p
      , drawListFn (scanr1 max) red maxRLabel p
      ]

-- TODO would be good to highlight the inputs to fold / zipWith as they are being used

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
    f x v = (textSVG_ def (show v)) # fc (col x) # lc (col x) # centerX <> square 1 # fc white # lc white # lw veryThick
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

waterHeightLabel ::
  Diagram B
waterHeightLabel =
  textSVG_ def "waterHeights" # fc purple # lc purple |||
  textSVG_ def " = zipWith min " # fc gray # lc gray |||
  textSVG_ def "maxL" # fc blue # lc blue |||
  textSVG_ def " " # fc gray # lc gray |||
  textSVG_ def "maxR" # fc red # lc red

drawProblemAndZipWithMin' ::
  Problem ->
  Int ->
  Diagram B
drawProblemAndZipWithMin' p@(Problem hs) i =
  let
    h = length hs
    vs = zipWith min (scanl1 max hs) (scanr1 max hs)
  in
    vsep space [
        drawZipWithLine' p vs purple i `atop`
        drawFoldr1MaxLine p h `atop`
        drawFoldl1MaxLine p h `atop`
        drawZipWithLine' p hs black h `atop`
        drawProblem p
      , drawHeightList p
      , drawListFn (scanl1 max) blue maxLLabel p
      , drawListFn (scanr1 max) red maxRLabel p
      , drawZipWith' vs purple waterHeightLabel i
      ]

drawProblemAndZipWithMin ::
  Problem ->
  [Diagram B]
drawProblemAndZipWithMin p@(Problem hs) =
  fmap (drawProblemAndZipWithMin' p) [0..length hs - 1]

drawWaterColumns ::
  Problem ->
  [Diagram B]
drawWaterColumns p@(Problem hs) =
  let
    gridHeight = 1 + maximum hs
    ws = waterHeights p
    col w h =
      if w == h
      then strutX 1
      -- TODO replace with a path that surrounds the whole area and fill?
      -- - this opens the door to merging those to demonstrate the sum step
      -- else vcat $ replicate (gridHeight - w) (strutY 1) ++ replicate (w - h) water ++ replicate h (strutY 1)
      else
        let
          path = [(1,0), (0, -1 * fromIntegral (w - h)), (-1,0)]
        in
          (fromOffsets . fmap r2 $ path)
            # closeLine
            # strokeLoop
            # translate (r2 (-0.5, 0.5 - fromIntegral (gridHeight - w)))
            # fc lightblue
            # lc white
            # lw veryThick
  in
    zipWith col ws hs

-- animate zipWith (-) heights
-- - animate the numbers while bringing in the volume blocks

volumesLabel ::
  Diagram B
volumesLabel =
  textSVG_ def "volumes" # fc lightblue # lc lightblue |||
  textSVG_ def " = zipWith (-) " # fc gray # lc gray |||
  textSVG_ def "waterHeights" # fc purple # lc purple |||
  textSVG_ def " " # fc gray # lc gray |||
  textSVG_ def "heights" # fc black # lc black

drawProblemAndZipWithSub' ::
  Problem ->
  Int ->
  Diagram B
drawProblemAndZipWithSub' p@(Problem hs) i =
  let
    h = length hs
    waterHeight = zipWith min (scanl1 max hs) (scanr1 max hs)
    volumes = zipWith (-) waterHeight hs
    waterColumns = hcat . take (i + 1) . drawWaterColumns $ p
  in
    vsep space [
        drawZipWithLine' p waterHeight purple h `atop`
        drawFoldr1MaxLine p h `atop`
        drawFoldl1MaxLine p h `atop`
        drawZipWithLine' p hs black h `atop`
        waterColumns `atop`
        drawProblem p
      , drawHeightList p
      , drawListFn (scanl1 max) blue maxLLabel p
      , drawListFn (scanr1 max) red maxRLabel p
      , drawZipWith' waterHeight purple waterHeightLabel h
      , drawZipWith' volumes lightblue volumesLabel i
      ]

drawProblemAndZipWithSub ::
  Problem ->
  [Diagram B]
drawProblemAndZipWithSub p@(Problem hs) =
  fmap (drawProblemAndZipWithSub' p) [0..length hs - 1]

drawPool ::
  Int ->
  Int ->
  [(Int, Int)] ->
  Diagram B
drawPool gridHeight offsetX heights =
  let
    diffs = fmap (uncurry (-)) heights
    firstDiff = case diffs of
      [] -> 0
      (h : _) -> h
    lastDiff = case reverse diffs of
      [] -> 0
      (h : _) -> h
    floors = fmap snd heights
    floors' = case floors of
      [] -> []
      h:t -> h : h : t
    floorDiffs = zipWith (-) floors floors'
    offsetY = case heights of
      [] -> 0
      (t,_) : _ -> gridHeight - t
    steps = intersperse (1, 0) . fmap (\y -> (0, fromIntegral y)) $ floorDiffs
    path = (0, -1 * fromIntegral firstDiff) : steps ++  [(1, 0), (0, fromIntegral lastDiff)]
  in
    (fromOffsets . fmap r2 $ path)
      # closeLine
      # strokeLoop
      # translate (r2 (-0.5 + fromIntegral offsetX, 0.5 - fromIntegral offsetY))
      # fc lightblue
      # lc white
      # lw veryThick

drawWater ::
  Problem ->
  Diagram B
drawWater (Problem hs) =
  let
    gridHeight = 1 + maximum hs
    waterHeight = zipWith min (scanl1 max hs) (scanr1 max hs)

    chunks = zip3 [0..] waterHeight hs

    groupPool (_,x1,x2) (_,y1,y2) = (x1 == x2) == (y1 == y2)

    nonEmptyPool [] = False
    nonEmptyPool ((_,x1,x2):_) = x1 /= x2

    stripOffset (_,x,y) = (x, y)

    shufflePool [] = []
    shufflePool xs@((h,_,_):_) = [(h, xs)]

    pools =
      fmap (fmap (fmap stripOffset)) .
      concatMap shufflePool .
      filter nonEmptyPool .
      groupBy groupPool $
      chunks
  in
    foldMap (uncurry (drawPool gridHeight)) pools

sumLabel ::
  Diagram B
sumLabel =
  textSVG_ def "answer = sum " # fc grey # lc grey |||
  textSVG_ def "volumes" # fc lightblue # lc lightblue

drawProblemAndSum ::
  Problem ->
  Diagram B
drawProblemAndSum p@(Problem hs) =
  let
    h = length hs
    waterHeight = zipWith min (scanl1 max hs) (scanr1 max hs)
    volumes = zipWith (-) waterHeight hs
  in
    vsep space [
        drawZipWithLine' p waterHeight purple h `atop`
        drawFoldr1MaxLine p h `atop`
        drawFoldl1MaxLine p h `atop`
        drawZipWithLine' p hs black h `atop`
        drawWater p `atop`
        drawProblem p
      , drawHeightList p
      , drawListFn (scanl1 max) blue maxLLabel p
      , drawListFn (scanr1 max) red maxRLabel p
      , drawZipWith' waterHeight purple waterHeightLabel h
      , drawZipWith' volumes lightblue volumesLabel h
      , drawListFn id white sumLabel p
      ]

diagrams ::
  Problem ->
  [Diagram B]
diagrams p =
  fmap ($ p) [drawProblem, drawSolution, drawProblemAndHeights] ++
  drawProblemAndFoldl1Max p ++
  [drawProblemAndScanl1Max p] ++
  drawProblemAndFoldr1Max p ++
  [drawProblemAndScanr1Max p] ++
  drawProblemAndZipWithMin p ++
  drawProblemAndZipWithSub p ++
  [drawProblemAndSum p]

-- add sum volume
-- - merge the volume blocks
--
-- optional: show that we can inline the code

