module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Waterflow

-- TODO change this to running through `Waterflow.diagrams` and creating an SVG for every diagram in the list
main :: IO ()
main =
  mainWith (drawProblemAndHeightList sampleProblem :: Diagram B)
