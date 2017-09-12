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
import Java.Model.JavaShow

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
  JavaShow a =>
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
  Eq      :: (Eq a, JavaShow a) => Expr v a -> Expr v a -> Expr v Bool
  Neq     :: (Eq a, JavaShow a) => Expr v a -> Expr v a -> Expr v Bool
  Lt      :: (Ord a, JavaShow a) => Expr v a -> Expr v a -> Expr v Bool
  Lte     :: (Ord a, JavaShow a) => Expr v a -> Expr v a -> Expr v Bool
  Gt      :: (Ord a, JavaShow a) => Expr v a -> Expr v a -> Expr v Bool
  Gte     :: (Ord a, JavaShow a) => Expr v a -> Expr v a -> Expr v Bool
  And     :: Expr v Bool -> Expr v Bool -> Expr v Bool
  Or      :: Expr v Bool -> Expr v Bool -> Expr v Bool
  Not     :: Expr v Bool -> Expr v Bool
  Add     :: (Num a, JavaShow a) => Expr v a -> Expr v a -> Expr v a
  Sub     :: (Num a, JavaShow a) => Expr v a -> Expr v a -> Expr v a
  Mul     :: (Num a, JavaShow a) => Expr v a -> Expr v a -> Expr v a
  Length  :: JavaShow a => Expr v [a] -> Expr v Int

{-
prec :: Expr v a -> Int
prec (Lit _) = 0
prec (ReadRef _) = 0
prec (Eq _ _) = 4
prec (Neq _ _) = 4
prec (Lt _ _) = 4
prec (Lte _ _) = 4
prec (Gt _ _) = 4
prec (Gte _ _) = 4
prec (And _ _) = 3
prec (Or _ _) = 2
prec (Not _) = 0
prec (Add _ _) = 6
prec (Sub _ _) = 6
prec (Mul _ _) = 7
prec (Length _) = 0

3 * 4 + 2
(3 * 4) + 2
3 * (4 + 2)
if the parent precedence is higher than our precedence, add parens

-}

writeBin ::
  JavaShow a =>
  Int ->
  Expr v a ->
  Expr v a ->
  String ->
  CodeWriter [CodeIndex]
writeBin d e1 e2 op = do
  ix1s <- writeExpr' d e1
  ix2 <- addChunk $ mconcat [" ", op, " "]
  ix3s <- writeExpr' d e2
  pure $ mconcat [ix1s, pure ix2, ix3s]

writeExpr ::
  JavaShow a =>
  Expr v a ->
  CodeWriter [CodeIndex]
writeExpr =
  writeExpr' 0

wrapParen ::
  Bool ->
  CodeWriter [CodeIndex] ->
  CodeWriter [CodeIndex]
wrapParen False cw = cw
wrapParen True cw = do
  _ <- addChunk "("
  res <- cw
  _ <- addChunk ")"
  pure res

writeExpr' ::
  JavaShow a =>
  Int ->
  Expr v a ->
  CodeWriter [CodeIndex]
writeExpr' _ (Lit x) = do
  ix <- addChunk . javaShow $ x
  pure [ix]
writeExpr' _ (ReadRef r) =
  writeRef r
writeExpr' d (Eq e1 e2) =
  wrapParen (d > 4) $
    writeBin 4 e1 e2 "=="
writeExpr' d (Neq e1 e2) =
  wrapParen (d > 4) $
    writeBin 4 e1 e2 "/="
writeExpr' d (Lt e1 e2) =
  wrapParen (d > 4) $
    writeBin 4 e1 e2 "<"
writeExpr' d (Lte e1 e2) =
  wrapParen (d > 4) $
    writeBin 4 e1 e2 "<="
writeExpr' d (Gt e1 e2) =
  wrapParen (d > 4) $
    writeBin 4 e1 e2 ">"
writeExpr' d (Gte e1 e2) =
  wrapParen (d > 4) $
    writeBin 4 e1 e2 ">="
writeExpr' d (And e1 e2) =
  wrapParen (d > 3) $
    writeBin 3 e1 e2 "&&"
writeExpr' d (Or e1 e2) =
  wrapParen (d > 2) $
    writeBin 2 e1 e2 "||"
writeExpr' d (Not e1) =
  wrapParen (d > 10) $ do
    ix0 <- addChunk "!"
    ix1s <- writeExpr' 10 e1
    pure $ mconcat [pure ix0, ix1s]
writeExpr' d (Add e1 e2) =
  wrapParen (d > 6) $
    writeBin 6 e1 e2 "+"
writeExpr' d (Sub e1 e2) =
  wrapParen (d > 6) $
    writeBin 6 e1 e2 "-"
writeExpr' d (Mul e1 e2) =
  wrapParen (d > 7) $
    writeBin 7 e1 e2 "*"
writeExpr' d (Length e1) =
  wrapParen (d > 10) $ do
    ix1s <- writeExpr' 10 e1
    ix2 <- addChunk ".length"
    pure $ mconcat [ix1s, pure ix2]

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
