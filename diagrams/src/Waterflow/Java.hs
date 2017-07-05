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

tHeights, tLeft, tRight, tLeftMax, tRightMax, tVolume :: Diagram B
tHeights = txt black "heights"
tLeft = txt blue "left"
tRight = txt red "right"
tLeftMax = txt darkblue "leftMax"
tRightMax = txt darkred "rightMax"
tVolume = txt lightblue "volume"

data Vars =
  Vars {
    _vLeft :: Int
  , _vRight :: Int
  , _vLeftMax :: Int
  , _vRightMax :: Int
  , _vVolume :: Int
  }

makeLenses ''Vars

initialVars ::
  Problem ->
  Vars
initialVars p =
  Vars 0 (pred . length . heights $ p) 0 0 0

-- reader for problem
-- state for vars
-- writer for list of segments

type ProgramRun m =
  ( MonadReader Problem m
  , MonadState Vars m
  , MonadWriter [([CodeIndex], Vars)] m
  )

runWhile ::
  ProgramRun m =>
  m ()
runWhile = do
  v <- get
  tell [([CodeIndex 6 1], v)]
  l <- use vLeft
  r <- use vRight
  if l < r
  then do
    runChangeLMax
    runChangeRMax
    runChangeVolume
    runWhile
  else pure ()

getHeightL ::
  ProgramRun m =>
  m Int
getHeightL = do
  li <- use vLeft
  hs <- asks heights
  return $ hs !! li

checkLMax ::
  ProgramRun m =>
  Int ->
  m Bool
checkLMax l = do
  v <- get
  tell [([CodeIndex 7 1], v)]
  lm <- use vLeftMax
  return $ l > lm

updateLMax ::
  ProgramRun m =>
  Int ->
  m ()
updateLMax l = do
  v <- get
  tell [([CodeIndex 8 1], v)]
  vLeftMax .= l
  v' <- get
  tell [([CodeIndex 8 1], v')]

runChangeLMax ::
  ProgramRun m =>
  m ()
runChangeLMax = do
  l <- getHeightL
  b <- checkLMax l
  when b $ updateLMax l

getHeightR ::
  ProgramRun m =>
  m Int
getHeightR = do
  ri <- use vRight
  hs <- asks heights
  return $ hs !! ri

checkRMax ::
  ProgramRun m =>
  Int ->
  m Bool
checkRMax r = do
  v <- get
  tell [([CodeIndex 10 1], v)]
  rm <- use vRightMax
  return $ r > rm

updateRMax ::
  ProgramRun m =>
  Int ->
  m ()
updateRMax r = do
  v <- get
  tell [([CodeIndex 11 1], v)]
  vRightMax .= r
  v' <- get
  tell [([CodeIndex 11 1], v')]

runChangeRMax ::
  ProgramRun m =>
  m ()
runChangeRMax = do
  r <- getHeightR
  b <- checkRMax r
  when b $ updateRMax r

checkSide ::
  ProgramRun m =>
  m Bool
checkSide = do
  v <- get
  tell [([CodeIndex 13 1], v)]
  lm <- use vLeftMax
  rm <- use vRightMax
  pure $ lm >= rm

addVolumeRight ::
  ProgramRun m =>
  m ()
addVolumeRight = do
  v <- get
  rm <- use vRightMax
  tell [([CodeIndex 14 2], v)]
  tell [([CodeIndex 14 1, CodeIndex 14 2], v)]
  ri <- use vRight
  hs <- asks heights
  let r = hs !! ri
  vVolume += rm - r
  v' <- get
  tell [([CodeIndex 14 1, CodeIndex 14 2], v')]

decRight ::
  ProgramRun m =>
  m ()
decRight = do
  v <- get
  tell [([CodeIndex 15 1], v)]
  vRight -= 1
  v' <- get
  tell [([CodeIndex 15 1], v')]

addVolumeLeft ::
  ProgramRun m =>
  m ()
addVolumeLeft = do
  v <- get
  lm <- use vLeftMax
  tell [([CodeIndex 17 2], v)]
  tell [([CodeIndex 17 1, CodeIndex 17 2], v)]
  li <- use vLeft
  hs <- asks heights
  let l = hs !! li
  vVolume += lm - l
  v' <- get
  tell [([CodeIndex 17 1, CodeIndex 17 2], v')]

incLeft ::
  ProgramRun m =>
  m ()
incLeft = do
  v <- get
  tell [([CodeIndex 18 1], v)]
  vLeft += 1
  v' <- get
  tell [([CodeIndex 18 1], v')]

runChangeVolume ::
  ProgramRun m =>
  m ()
runChangeVolume = do
  b <- checkSide
  if b
  then do
    addVolumeRight
    decRight
  else do
    addVolumeLeft
    incLeft

runReturn ::
  ProgramRun m =>
  m ()
runReturn = do
  v <- get
  tell [([CodeIndex 21 1], v)]
  return ()

runProgram ::
  ( MonadReader Problem m
  , MonadState Vars m
  , MonadWriter [([CodeIndex], Vars)] m
  ) =>
  m ()
runProgram = do
  runWhile
  runReturn

results ::
  Problem ->
  [([CodeIndex], Vars)]
results p =
  flip runReader p .
  flip evalStateT (initialVars p) .
  execWriterT $
  runProgram

drawResult ::
  Problem ->
  [CodeIndex] ->
  Vars ->
  Diagram B
drawResult p cis vs =
  let
    code = drawCode cis
  in
    code ||| annotateProblem p vs -- # sized (mkHeight (height code))

data CodeIndex = CodeIndex Int Int

drawCode ::
  [CodeIndex] ->
  Diagram B
drawCode ixes =
  let
    m = Map.fromList . zip [0..]
    s = m . pure
    segments = m [
        s $ t "int " ||| tLeft ||| t " = 0;"
      , s $ t "int " ||| tRight ||| t " = tHeights.length - 1;"
      , s $ t "int " ||| tLeftMax ||| t " = 0;"
      , s $ t "int " ||| tRightMax ||| t " = 0;"
      , s $ t "int " ||| tVolume ||| t " = 0;"
      , s $ t " "
      -- CodeIndex 6 1
      , m [ t "while ( ", tLeft ||| t " < " ||| tRight, t " ) {"]
      -- CodeIndex 7 1
      , m [ t "  if ( " , tHeights ||| t "[" ||| tLeft ||| t "] > " ||| tLeftMax , t " ) {"]
      -- CodeIndex 8 1
      , m [ t "    " , tLeftMax ||| t " = " ||| tHeights ||| t "[" ||| tLeft , t "];"]
      , s $ t "  }"
      -- CodeIndex 10 1
      , m [ t "  if ( " , tHeights ||| t "[" ||| tRight ||| t "] > " ||| tRightMax,  t " ) {"]
      -- CodeIndex 11 1
      , m [ t "    " , tRightMax ||| t " = " ||| tHeights ||| t "[" ||| tRight , t "];"]
      , s $ t "  }"
      -- CodeIndex 13 1
      , m [ t "  if ( " , tLeftMax ||| t " >= " ||| tRightMax , t " ) {"]
      -- CodeIndex 14 1 and 14 2 -- maybe we should take a list of segments to highlight
      -- we want 14 2 and then 14 1 + 14 2
      , m [t "    " , tVolume ||| t " += ", tRightMax ||| t " - " ||| tHeights ||| t "[" ||| tRight ||| t "]", t ";"]
      -- CodeIndex 15 1
      , m [ t "    " , tRight ||| t "--;"]
      , s $ t "  } else {"
      -- CodeIndex 17 1 and 17 2
      , m [t "    " , tVolume ||| t " += " , tLeftMax ||| t " - " ||| tHeights ||| t "[" ||| tLeft ||| t "]", t ";"]
      -- CodeIndex 18 1
      , m [ t "    " , tLeft ||| t "++;"]
      , s $ t "  }"
      , s $ t "}"
      -- CodeIndex 21 1
      , m [ t "return " , tVolume , t ";"]
      ]
    hlColour = lightpink

    -- TODO this could be done better
    hl :: Diagram B -> Diagram B
    hl d = d # alignL <> square 1 # alignL # scaleX (width d) # scaleY (height d) # fc hlColour # lc hlColour

    f (CodeIndex i j) =
      Map.adjust (Map.adjust hl j) i

    segments' = foldr f segments ixes

    collapse :: Map Int (Map Int k) -> [[k]]
    collapse = fmap Map.elems . Map.elems
  in
    vcat . fmap hcat . collapse $ segments'

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
    hsep space [row, tVolume]

labelLeftMax ::
  Problem ->
  Int ->
  Diagram B
labelLeftMax p lm =
  let
    gridHeight = 1 + maximum (heights p)
    yOffset = 0.5 + (-1 * fromIntegral (gridHeight - lm))
  in
    txt darkblue "LM" # centerY # translateY yOffset

labelRightMax ::
  Problem ->
  Int ->
  Diagram B
labelRightMax p rm =
  let
    gridHeight = 1 + maximum (heights p)
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
  Problem ->
  Vars ->
  Diagram B
drawProblem' p@(Problem hs) (Vars l r lm rm _) =
  let
    gridHeight = 1 + maximum hs
    gridWidth = length hs
    lLeft = labelLeftMax p lm
    lRight = labelRightMax p rm
    problemWithLines = mconcat [
        drawCircle l (gridHeight - hs !! l) blue
      , drawVLine gridHeight l blue
      , drawHLine gridWidth (gridHeight - lm) darkblue
      , drawCircle r (gridHeight - hs !! r) red
      , drawVLine gridHeight r red
      , drawHLine gridWidth (gridHeight - rm) darkred
      , drawProblem p
      ]
  in
    beside (r2 (1, 0)) (beside (r2 (-1, 0)) problemWithLines (lLeft ||| strutX space)) (strutX space ||| lRight)

drawHeightList' ::
  Problem ->
  Diagram B
drawHeightList' p =
  let
    row = hcat . fmap (onSquare . txtShow black) . heights $ p
  in
    hsep space [row, tHeights]

annotateProblem ::
  Problem ->
  Vars ->
  Diagram B
annotateProblem p vars@(Vars l r lm rm v) =
  let
    h = length . heights $ p
    pieces = [
        drawProblem' p vars
      , drawLeftAndRight h l r
      , drawVolume h v
      ]
  in
    vsep space pieces

javaDiagrams ::
  Problem ->
  [Diagram B]
javaDiagrams p =
  drawResult p [] (initialVars p) :
  fmap (uncurry $ drawResult p) (results p)
