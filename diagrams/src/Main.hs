{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (zipWithM_)

import System.Directory

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Waterflow

mkSameSize ::
  [Diagram B] ->
  [Diagram B]
mkSameSize ds =
  let
    maxW = maximum . map width $ ds
    maxH = maximum . map height $ ds
    adjust d = (d ||| strutX (maxW  - width d)) === strutY (maxH - height d)
  in
    fmap adjust ds

main :: IO ()
main =
  let
    ds = mkSameSize . diagrams $ sampleProblem
    render i = renderSVG ("./images/image" ++ show i ++ ".svg") (mkHeight 800)
  in do
    createDirectoryIfMissing False "./images"
    zipWithM_ render [0..] ds
