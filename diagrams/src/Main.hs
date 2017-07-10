{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad (zipWithM_)
import Data.Bool(bool)

import Control.Monad.Writer

import System.Directory

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Waterflow.Haskell (haskellDiagrams)
import Waterflow.Java (javaDiagrams)
import Waterflow.Common (sampleProblem, commonDiagrams)

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

mkFileName ::
  String ->
  Int ->
  String
mkFileName n i =
  let
    number = bool id ('0':) <$> (9>) <*> show
  in
    "./images/" ++ n ++ number i ++ ".svg"

mkSlide ::
  String ->
  [String]
mkSlide fileName =
  [ "##\n\n"
  , "![](" ++ fileName ++ ")\n\n"
  ]

renderImage ::
  ( MonadIO m
  , MonadWriter [String] m
  ) =>
  String ->
  Diagram B ->
  m ()
renderImage f d = do
  tell $ mkSlide f
  liftIO $ renderSVG f (mkHeight 800) d

main :: IO ()
main =
  let
    cds = mkSameSize . commonDiagrams $ sampleProblem
    hds = mkSameSize . haskellDiagrams $ sampleProblem
    jds = mkSameSize . javaDiagrams $ sampleProblem
    render' n i =
      renderImage (mkFileName n i)
  in do
    createDirectoryIfMissing False "./images"
    slides <- execWriterT $ do
      tell ["% Why Functional Programming?\n", "% Tony Morris\n"]

      tell ["# An example of a programming problem \n"]
      zipWithM_ (render' "problem") [0..] cds

      tell ["# The not Functional Programming solution\n"]
      zipWithM_ (render' "java") [0..] jds

      tell ["# The Functional Programming solution\n"]
      zipWithM_ (render' "haskell") [0..] hds

    writeFile "./slides.md" (mconcat slides)
