{-# LANGUAGE TemplateHaskell #-}
module Java.Model.Test (
    waterflowDiagrams
  , testDiagrams
  ) where

import Control.Lens

import Diagrams.Prelude
import Diagrams.Backend.SVG

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

drawWVars :: WaterflowVars -> Diagram B
drawWVars (WaterflowVars heights left right leftMax rightMax volume) =
  hsep 1.0 . fmap t $ [show heights, show left, show right, show leftMax, show rightMax, show volume]

waterflowDiagrams :: Problem -> [Diagram B]
waterflowDiagrams p@(Problem hs) =
  animateProgram (WaterflowVars hs 0 0 0 0 0) (runStm $ waterflow p) drawWVars

data TestVars =
  TestVars {
    _tvA :: Int
  , _tvB :: Bool
  , _tvC :: Int
  , _tvD :: [Int]
  , _tvE :: [[Int]]
  } deriving (Eq, Show)

makeLenses ''TestVars

va :: Var TestVars Int
va = Var "a" blue "int" tvA

vb :: Var TestVars Bool
vb = Var "b" red "bool" tvB

vc :: Var TestVars Int
vc = Var "c" green "int" tvC

vd :: Var TestVars [Int]
vd = Var "d" yellow "int []" tvD

ve :: Var TestVars [[Int]]
ve = Var "e" gray "int [[]]" tvE

setup :: Int -> Stm TestVars ()
setup x =
  DeclVar va (Lit 0) `Seq`
  DeclVar vb (Lit False) `Seq`
  DeclVar vc (Lit x) `Seq`
  DeclVar vd (Lit [1, 2]) `Seq`
  DeclVar ve (Lit [[3], [4, 5]])

whileBody :: Stm TestVars ()
whileBody =
  WriteRef (RVar va) (Add (ReadRef (RVar va)) (Lit 1)) `Seq`
  WriteRef (RIx (RVar vd) (Lit 1)) (Add (ReadRef (RIx (RVar vd) (Lit 1))) (Lit 1)) `Seq`
  WriteRef (RVar vc) (Sub (ReadRef (RVar vc)) (Lit 1))

test :: Int -> Stm TestVars Int
test x =
  setup x `Seq`
  While (Lte (ReadRef (RVar va)) (ReadRef (RVar vc))) whileBody `Seq`
  Return (ReadRef (RVar va))

drawV :: TestVars -> Diagram B
drawV (TestVars a b c d e) =
  hcat . fmap t $ [show a, show b, show c, show d, show e]

testDiagrams :: [Diagram B]
testDiagrams =
  animateProgram (TestVars 0 False 0 [0,0] [[0], [0,0]]) (runStm $ test 5) drawV
