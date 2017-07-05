{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Waterflow.Haskell (
    haskellDiagrams
  ) where

import Data.List (intersperse, groupBy)

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Waterflow.Common

tHeights, tMaxL, tMaxR, tWaterHeights, tVolumes :: Diagram B
tHeights = txt black "heights"
tMaxL = txt blue "maxL"
tMaxR = txt red "maxR"
tWaterHeights = txt purple "waterHeights"
tVolumes = txt lightblue "volumes"

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
    f x = onSquare . txtShow (col x) 
    s = scanl1 max hs
  in
    hcat (zipWith f [0..] s) |||
    strutX space |||
    t "foldl1 max"

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
  hcat [tMaxL,  t " = scanl1 max ", tHeights]

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
    f x = onSquare . txtShow (col x)
    s = scanr1 max hs
  in
    hcat (reverse . zipWith f [0..] . reverse $ s) |||
    strutX space |||
    t "foldr1 max"

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
  hcat [tMaxR, t " = scanr1 max ", tHeights]

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

waterHeightLabel ::
  Diagram B
waterHeightLabel =
  hcat [tWaterHeights, t " = zipWith min ", tMaxL, t " ", tMaxR]

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
  hcat [tVolumes, t " = zipWith (-) ", tWaterHeights, t " ", tHeights]

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
  hcat [t "answer = sum ", tVolumes]

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

haskellDiagrams ::
  Problem ->
  [Diagram B]
haskellDiagrams p =
  fmap ($ p) [drawProblem, drawSolution, drawProblemAndHeights] ++
  drawProblemAndFoldl1Max p ++
  [drawProblemAndScanl1Max p] ++
  drawProblemAndFoldr1Max p ++
  [drawProblemAndScanr1Max p] ++
  drawProblemAndZipWithMin p ++
  drawProblemAndZipWithSub p ++
  [drawProblemAndSum p]

-- Tools for a potential tidy up

data Scene =
  Scene {
    lines :: Problem -> Diagram B
  , overlay :: Problem -> Diagram B
  , labels :: Problem -> [Diagram B]
  }

instance Monoid Scene where
  mempty =
    Scene (const mempty) (const mempty) (const mempty)
  mappend (Scene li1 _ la1) (Scene li2 o2 la2) =
    Scene (mappend li2 li1) o2 (mappend la1 la2)

-- probably also want IndexedScene

drawScene ::
  Scene ->
  Problem ->
  Diagram B
drawScene (Scene li o la) p =
  vsep space $ mconcat [li p, o p, drawProblem p] : la p

problemScene ::
  Scene
problemScene = mempty

solutionScene ::
  Scene
solutionScene =
  Scene mempty drawWaterSquares (const [])

problemAndHeightsScene ::
  Scene
problemAndHeightsScene =
  let
    heightLine p =
      let
        hs = heights p
        h = length hs
      in
        drawZipWithLine' p hs black h
  in
    Scene heightLine mempty (pure . drawHeightList)
