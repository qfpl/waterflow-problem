{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (zipWithM_)
import Data.Bool(bool)

import System.Directory

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Waterflow.Haskell (haskellDiagrams)
import Waterflow.Java (javaDiagrams)
import Waterflow.Common (sampleProblem)

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
    hds = mkSameSize . haskellDiagrams $ sampleProblem
    jds = mkSameSize . javaDiagrams $ sampleProblem
    number = bool id ('0':) <$> (9>) <*> show

    render n i = renderSVG ("./images/" ++ n ++ number i ++ ".svg") (mkHeight 800)
  in do
    createDirectoryIfMissing False "./images"
    zipWithM_ (render "haskell") [0..] hds
    zipWithM_ (render "java") [0..] jds
