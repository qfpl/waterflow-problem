{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Java.Program (
    Var(..)
  , CodeIndex
  , CodeWriter
  , indent
  , addChunk
  , addVarChunk
  , addNewLine
  , RunTrace(..)
  , CodeRunner
  , Program
  , animateProgram
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Lens hiding ((#))
import Control.Monad.State (StateT, MonadState(..), evalStateT)
import Control.Monad.Writer (WriterT, MonadWriter(..), runWriterT)

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Waterflow.Common

data Var v a =
  Var {
    vName  :: String
  , vColor :: Colour Double
  , vType  :: String
  , vLens  :: Lens' v a
  }

data Chunk =
  Chunk {
    cText :: String
  , cDiagram :: Diagram B
  }

instance Eq Chunk where
  (Chunk s1 _) == (Chunk s2 _) = s1 == s2

instance Show Chunk where
  show (Chunk s _) = s

c :: String -> Chunk
c s = Chunk s (t s)

newtype CodeChunks = CodeChunks [[Chunk]]
  deriving (Eq, Show)

instance Monoid CodeChunks where
  mempty =
    CodeChunks mempty
  mappend (CodeChunks cc1) (CodeChunks cc2) =
    CodeChunks (mappend cc1 cc2)

data CodeIndex =
  CodeIndex {
    _ciRow :: Int
  , _ciColumn :: Int
  } deriving (Eq, Show)

makeLenses ''CodeIndex

initialCodeIndex ::
  CodeIndex
initialCodeIndex =
  CodeIndex 0 0

data CodeWriterState =
  CodeWriterState {
    _cwsIndent :: Int
  , _cwsCurrentIndex :: CodeIndex
  , _cwsCurrentLine :: [Chunk]
  }

makeLenses ''CodeWriterState

initialCodeWriterState ::
  CodeWriterState
initialCodeWriterState =
  CodeWriterState 0 initialCodeIndex []

newtype CodeWriterT m a =
  CodeWriterT { unCodeWriterT :: StateT CodeWriterState (WriterT CodeChunks m) a}
  deriving (Functor, Applicative, Monad, MonadState CodeWriterState, MonadWriter CodeChunks)

indent ::
  Monad m =>
  CodeWriterT m a ->
  CodeWriterT m a
indent ma = do
  cwsIndent += 1
  cwsCurrentIndex . ciColumn += 1
  r <- ma
  addNewLine
  cwsIndent -= 1
  cwsCurrentIndex . ciColumn -= 1
  pure r

addChunk' ::
  Monad m =>
  Chunk ->
  CodeWriterT m CodeIndex
addChunk' c = do
  cwsCurrentLine %= (c :)
  r <- use cwsCurrentIndex
  cwsCurrentIndex . ciColumn += 1
  pure r

addChunk ::
  Monad m =>
  String ->
  CodeWriterT m CodeIndex
addChunk =
  addChunk' . c

addVarChunk ::
  Monad m =>
  Var v a ->
  CodeWriterT m CodeIndex
addVarChunk v =
  addChunk' $ Chunk (vName v) (txt (vColor v) (vName v))

addNewLine ::
  Monad m =>
  CodeWriterT m ()
addNewLine = do
  indent <- use cwsIndent
  cl <- use cwsCurrentLine
  tell $ CodeChunks [replicate indent (c "  ") ++ reverse cl]
  cwsCurrentLine .= []
  cwsCurrentIndex . ciColumn .= indent
  cwsCurrentIndex . ciRow += 1
  pure ()

runCodeWriterT :: Monad m => CodeWriterT m a -> m (a, CodeChunks)
runCodeWriterT =
  runWriterT .
  flip evalStateT initialCodeWriterState .
  unCodeWriterT

type CodeWriter = CodeWriterT Identity

runCodeWriter :: CodeWriter a -> (a, CodeChunks)
runCodeWriter =
  runIdentity .
  runCodeWriterT

data RunTrace v =
  RunTrace {
    rtVars :: v
  , rtHighlights :: [CodeIndex]
  } deriving (Eq, Show)

newtype CodeRunnerT v m a =
  CodeRunnerT { unCodeRunnerT :: StateT v (WriterT [RunTrace v] m) a}
  deriving (Functor, Applicative, Monad, MonadState v, MonadWriter [RunTrace v])

type CodeRunner v = CodeRunnerT v Identity

runCodeRunnerT ::
  Monad m =>
  v ->
  CodeRunnerT v m a ->
  m (a, [RunTrace v])
runCodeRunnerT v =
  runWriterT .
  flip evalStateT v .
  unCodeRunnerT

runCodeRunner ::
  v ->
  CodeRunner v a ->
  (a, [RunTrace v])
runCodeRunner v =
  runIdentity .
  runCodeRunnerT v

type Program v a = CodeWriter (CodeRunner v a)

runProgram :: v -> Program v a -> (a, CodeChunks, [RunTrace v])
runProgram v p =
  let
    (cr, chunks) = runCodeWriter p
    (a, traces) = runCodeRunner v cr
  in
    (a, chunks, traces)

drawCode ::
  (v -> Diagram B) ->
  Map Int (Map Int (Diagram B)) ->
  RunTrace v ->
  Diagram B
drawCode drawVar chunks (RunTrace v ixs) =
  let
    hlColour = lightpink
    hl d = d # bg hlColour
    f (CodeIndex i j) = Map.adjust (Map.adjust hl j) i
    chunks' = foldr f chunks ixs
    collapse = fmap Map.elems . Map.elems
    code = vcat . fmap hcat . collapse $ chunks'
  in
    code ||| drawVar v

animateCode ::
  (v -> Diagram B) ->
  CodeChunks ->
  [RunTrace v] ->
  [Diagram B]
animateCode drawVar (CodeChunks chunks) =
  let
    m = Map.fromList . zip [0..]
    chunks' = m . fmap (m . fmap cDiagram) $ chunks
  in
    fmap (drawCode drawVar chunks')

animateProgram ::
  v ->
  Program v a ->
  (v -> Diagram B) ->
  [Diagram B]
animateProgram v p drawVar =
  let
    (_, chunks, traces) = runProgram v p
  in
    animateCode drawVar chunks traces
