{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Java.Model.Expr (
    Ref(..)
  , ref
  , writeRef
  , runRef
  , Expr(..)
  , writeExpr
  , runExpr
  , runExprToLit
  ) where

import Control.Lens

import Java.Program

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
  lift2 (\_ ix' -> ReadRef (RIx r ix')) (!!) <$> runRef r <*> runExpr ix

data Expr v a where
  Lit     :: a -> Expr v a
  ReadRef :: Ref v a -> Expr v a
  Eq      :: (Eq a, Show a) => Expr v a -> Expr v a -> Expr v Bool
  Neq     :: (Eq a, Show a) => Expr v a -> Expr v a -> Expr v Bool
  Lt      :: (Ord a, Show a) => Expr v a -> Expr v a -> Expr v Bool
  Lte     :: (Ord a, Show a) => Expr v a -> Expr v a -> Expr v Bool
  Gt      :: (Ord a, Show a) => Expr v a -> Expr v a -> Expr v Bool
  Gte     :: (Ord a, Show a) => Expr v a -> Expr v a -> Expr v Bool
  And     :: Expr v Bool -> Expr v Bool -> Expr v Bool
  Or      :: Expr v Bool -> Expr v Bool -> Expr v Bool
  Not     :: Expr v Bool -> Expr v Bool
  Add     :: (Num a, Show a) => Expr v a -> Expr v a -> Expr v a
  Sub     :: (Num a, Show a) => Expr v a -> Expr v a -> Expr v a
  Mul     :: (Num a, Show a) => Expr v a -> Expr v a -> Expr v a
  Length  :: Show a => Expr v [a] -> Expr v Int

writeBin ::
  Show a =>
  Expr v a ->
  Expr v a ->
  String ->
  CodeWriter [CodeIndex]
writeBin e1 e2 op = do
  ix0 <- addChunk "("
  ix1s <- writeExpr e1
  ix2 <- addChunk $ mconcat [" ", op, " "]
  ix3s <- writeExpr e2
  ix4 <- addChunk ")"
  pure $ mconcat [pure ix0, ix1s, pure ix2, ix3s, pure ix4]

writeExpr ::
  Show a =>
  Expr v a ->
  CodeWriter [CodeIndex]
writeExpr (Lit x) = do
  ix <- addChunk . show $ x
  pure [ix]
writeExpr (ReadRef r) = do
  writeRef r
writeExpr (Eq e1 e2) =
  writeBin e1 e2 "=="
writeExpr (Neq e1 e2) =
  writeBin e1 e2 "/="
writeExpr (Lt e1 e2) =
  writeBin e1 e2 "<"
writeExpr (Lte e1 e2) =
  writeBin e1 e2 "<="
writeExpr (Gt e1 e2) =
  writeBin e1 e2 ">"
writeExpr (Gte e1 e2) =
  writeBin e1 e2 ">="
writeExpr (And e1 e2) =
  writeBin e1 e2 "&&"
writeExpr (Or e1 e2) =
  writeBin e1 e2 "||"
writeExpr (Not e1) = do
  ix0 <- addChunk "!("
  ix1s <- writeExpr e1
  ix2 <- addChunk ")"
  pure $ mconcat [pure ix0, ix1s, pure ix2]
writeExpr (Add e1 e2) =
  writeBin e1 e2 "+"
writeExpr (Sub e1 e2) =
  writeBin e1 e2 "-"
writeExpr (Mul e1 e2) =
  writeBin e1 e2 "*"
writeExpr (Length e1) = do
  ix0 <- addChunk "("
  ix1s <- writeExpr e1
  ix2 <- addChunk ".length)"
  pure $ mconcat [pure ix0, ix1s, pure ix2]

lift1 ::
  (Expr v a -> Expr v b) ->
  (a -> b) ->
  Expr v a ->
  Expr v b
lift1 _ f (Lit l1) =
  Lit (f l1)
lift1 f _ e1 =
  f e1

lift2 ::
  (Expr v a -> Expr v b -> Expr v c) ->
  (a -> b -> c) ->
  Expr v a ->
  Expr v b ->
  Expr v c
lift2 _ f (Lit l1) (Lit l2) =
  Lit (f l1 l2)
lift2 f _ e1 e2 =
  f e1 e2

runExpr ::
  Expr v a ->
  CodeRunner v (Expr v a)
runExpr (Lit x) =
  pure $ Lit x
runExpr (ReadRef r) =
  runRef r
runExpr (Eq e1 e2) =
  lift2 Eq (==) <$> runExpr e1 <*> runExpr e2
runExpr (Neq e1 e2) =
  lift2 Neq (/=) <$> runExpr e1 <*> runExpr e2
runExpr (Lt e1 e2) =
  lift2 Lt (<) <$> runExpr e1 <*> runExpr e2
runExpr (Lte e1 e2) =
  lift2 Lte (<=) <$> runExpr e1 <*> runExpr e2
runExpr (Gt e1 e2) =
  lift2 Gt (>) <$> runExpr e1 <*> runExpr e2
runExpr (Gte e1 e2) =
  lift2 Gte (>=) <$> runExpr e1 <*> runExpr e2
runExpr (And e1 e2) =
  lift2 And (&&) <$> runExpr e1 <*> runExpr e2
runExpr (Or e1 e2) =
  lift2 Or (||) <$> runExpr e1 <*> runExpr e2
runExpr (Not e1) =
  lift1 Not not <$> runExpr e1
runExpr (Add e1 e2) =
  lift2 Add (+) <$> runExpr e1 <*> runExpr e2
runExpr (Sub e1 e2) =
  lift2 Sub (-) <$> runExpr e1 <*> runExpr e2
runExpr (Mul e1 e2) =
  lift2 Mul (*) <$> runExpr e1 <*> runExpr e2
runExpr (Length e1) =
  lift1 Length length <$> runExpr e1

runExprToLit ::
  Expr v a ->
  CodeRunner v a
runExprToLit e = do
  e' <- runExpr e
  case e' of
    Lit x -> pure x
    _ -> error "fix me with MonadError"
