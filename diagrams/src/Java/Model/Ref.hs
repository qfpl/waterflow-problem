module Java.Model.Ref (
    Ref(..)
  , ref
  , writeRef
  , runRef
  ) where

import Control.Lens

import Java.Program (Var(..))

data Ref v a where
  RVar :: Var v a -> Ref v a
  RIx  :: Ref v [a] -> Expr v Int -> Ref v a

ref ::
  Ref v a ->
  Traversal' v a
ref (RVar v) =
  vLens v
ref (RIx r (Lit i)) =
  ref r . ix i
ref (RIx r _) =
  ignored

writeRef ::
  Show a =>
  Ref v a ->
  CodeWriter [CodeIndex]
writeRef (RVar v) = do
  ix <- addVarChunk v
  pure [ix]
writeRef (RIx r ix) = do
  ixRef <- writeRef r
  ixL <- addChunk "["
  ixI <- writeExpr ix
  ixR <- addChunk "]"
  pure $ mconcat [ixRef, pure ixL, ixI, pure ixR]

runRef ::
  Ref v a ->
  CodeRunner v (Expr v a)
runRef (RVar v) = do
  x <- use $ vLens v
  pure $ Lit x
runRef (RIx r ix) = do
  -- TODO add bounds info, check for out of bounds, throw errors rather than using a partial function here
  lift2 (\_ ix' -> ReadRef (RIx r ix')) (!!) <$> evalRef r <*> evalExpr ix
