{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Java.Model.Stm (
    Stm(..)
  , runStm
  ) where

import Control.Lens

import Control.Monad.State (get)
import Control.Monad.Writer (tell)

import Java.Program
import Java.Model.Expr

data Stm v a where
  DeclVar  :: Show a => Var v a -> Expr v a -> Stm v ()
  WriteRef :: Show a => Ref v a -> Expr v a -> Stm v ()

  Seq      :: Show a => Stm v a -> Stm v b -> Stm v b
  While    :: Show a => Expr v Bool -> Stm v a -> Stm v ()
  If       :: Show a => Expr v Bool -> Stm v a -> Stm v ()
  IfElse   :: (Show a, Show b) => Expr v Bool -> Stm v a -> Stm v b -> Stm v ()

  PreInc   :: (Show a, Num a) => Ref v a -> Stm v a
  PostInc  :: (Show a, Num a) => Ref v a -> Stm v a
  PreDec   :: (Show a, Num a) => Ref v a -> Stm v a
  PostDec  :: (Show a, Num a) => Ref v a -> Stm v a
  WriteAdd :: (Show a, Num a) => Ref v a -> Expr v a -> Stm v ()
  WriteSub :: (Show a, Num a) => Ref v a -> Expr v a -> Stm v ()
  WriteMul :: (Show a, Num a) => Ref v a -> Expr v a -> Stm v ()

  Return  :: Expr v a -> Stm v a

runSet ::
  forall v a.
  [CodeIndex] ->
  [CodeIndex] ->
  Ref v a ->
  Expr v a ->
  CodeRunner v ()
runSet ixAll ixE r e = do
  v <- get
  tell . pure $ RunTrace v ixE
  e' <- runExprToLit e
  ref r .= e'
  v' <- get
  tell . pure $ RunTrace v' ixAll
  pure ()

runSeq ::
  CodeRunner v a ->
  CodeRunner v b ->
  CodeRunner v b
runSeq s1 s2 = do
  _ <- s1
  s2

runWhile ::
  [CodeIndex] ->
  Expr v Bool ->
  CodeRunner v a ->
  CodeRunner v ()
runWhile bIxs b s = do
  v <- get
  tell . pure $ RunTrace v bIxs
  b' <- runExprToLit b
  case b' of
    True -> do
      s
      runWhile bIxs b s
    _ -> pure ()

runIf ::
  [CodeIndex] ->
  Expr v Bool ->
  CodeRunner v a ->
  CodeRunner v ()
runIf bIxs b t = do
  v <- get
  tell . pure $ RunTrace v bIxs
  b' <- runExprToLit b
  case b' of
    True -> () <$ t
    _ -> pure ()

runIfElse ::
  [CodeIndex] ->
  Expr v Bool ->
  CodeRunner v a ->
  CodeRunner v b ->
  CodeRunner v ()
runIfElse bIxs b t f = do
  v <- get
  tell . pure $ RunTrace v bIxs
  b' <- runExprToLit b
  case b' of
    True -> () <$ t
    False -> () <$ f

runPreInc ::
  Num a =>
  [CodeIndex] ->
  Ref v a ->
  CodeRunner v a
runPreInc ix r = do
  v <- get
  tell . pure $ RunTrace v ix
  ref r += 1
  e <- runRef r
  x <- runExprToLit e
  v' <- get
  tell . pure $ RunTrace v' ix
  pure x

runPostInc ::
  Num a =>
  [CodeIndex] ->
  Ref v a ->
  CodeRunner v a
runPostInc ix r = do
  v <- get
  tell . pure $ RunTrace v ix
  e <- runRef r
  x <- runExprToLit e
  ref r += 1
  v' <- get
  tell . pure $ RunTrace v' ix
  pure x

runPreDec ::
  Num a =>
  [CodeIndex] ->
  Ref v a ->
  CodeRunner v a
runPreDec ix r = do
  v <- get
  tell . pure $ RunTrace v ix
  ref r -= 1
  e <- runRef r
  x <- runExprToLit e
  v' <- get
  tell . pure $ RunTrace v' ix
  pure x

runPostDec ::
  Num a =>
  [CodeIndex] ->
  Ref v a ->
  CodeRunner v a
runPostDec ix r = do
  v <- get
  tell . pure $ RunTrace v ix
  e <- runRef r
  x <- runExprToLit e
  ref r -= 1
  v' <- get
  tell . pure $ RunTrace v' ix
  pure x

runWriteAdd ::
  Num a =>
  [CodeIndex] ->
  [CodeIndex] ->
  Ref v a ->
  Expr v a ->
  CodeRunner v ()
runWriteAdd ixR ixE r e = do
  v <- get
  tell . pure $ RunTrace v ixR
  tell . pure $ RunTrace v ixE

  x <- runExprToLit e
  ref r += x

  v' <- get
  tell . pure $ RunTrace v' ixE
  pure ()

runWriteSub ::
  Num a =>
  [CodeIndex] ->
  [CodeIndex] ->
  Ref v a ->
  Expr v a ->
  CodeRunner v ()
runWriteSub ixR ixE r e = do
  v <- get
  tell . pure $ RunTrace v ixR
  tell . pure $ RunTrace v ixE

  x <- runExprToLit e
  ref r -= x

  v' <- get
  tell . pure $ RunTrace v' ixE
  pure ()

runWriteMul ::
  Num a =>
  [CodeIndex] ->
  [CodeIndex] ->
  Ref v a ->
  Expr v a ->
  CodeRunner v ()
runWriteMul ixR ixE r e = do
  v <- get
  tell . pure $ RunTrace v ixR
  tell . pure $ RunTrace v ixE

  x <- runExprToLit e
  ref r *= x

  v' <- get
  tell . pure $ RunTrace v' ixE
  pure ()

runReturn ::
  [CodeIndex] ->
  Expr v a ->
  CodeRunner v a
runReturn ixs r = do
  v <- get
  tell . pure $ RunTrace v ixs
  runExprToLit r

runStm ::
  Show a =>
  Stm v a ->
  Program v a
runStm s = do
  x <- runStm' s
  addNewLine
  pure x

runStm' ::
  Show a =>
  Stm v a ->
  Program v a
runStm' (DeclVar v e) = do
  _ <- addChunk $ vType v
  _ <- addChunk " "
  ixV <- addVarChunk v
  ixEq <- addChunk " = "
  ixE <- writeExpr e
  addChunk ";"
  addNewLine
  pure $ runSet (mconcat [pure ixV, pure ixEq, ixE]) ixE (RVar v) e
runStm' (WriteRef r e) = do
  ixR <- writeRef r
  ixEq <- addChunk " = "
  ixE <- writeExpr e
  addChunk ";"
  pure $ runSet (mconcat [ixR, pure ixEq, ixE]) ixE r e
runStm' (Seq s1 s2) = do
  s1' <- runStm' s1
  addNewLine
  s2' <- runStm' s2
  pure $ runSeq s1' s2'
runStm' (While b s) = do
  _ <- addChunk "while ( "
  bIxs <- writeExpr b
  _ <- addChunk " ) {"
  addNewLine
  s' <- indent $ runStm' s
  _ <- addChunk "}"
  pure $ runWhile bIxs b s'
runStm' (If b t) = do
  _ <- addChunk "if ( "
  bIxs <- writeExpr b
  _ <- addChunk " ) {"
  addNewLine
  t' <- indent $ runStm' t
  _ <- addChunk "}"
  pure $ runIf bIxs b t'
runStm' (IfElse b t f) = do
  _ <- addChunk "if ( "
  bIxs <- writeExpr b
  _ <- addChunk " ) {"
  addNewLine
  t' <- indent $ runStm' t
  _ <- addChunk "} else {"
  addNewLine
  f' <- indent $ runStm' f
  _ <- addChunk "}"
  pure $ runIfElse bIxs b t' f'
runStm' (PreInc r) = do
  _ <- addChunk "++"
  ix <- writeRef r
  _ <- addChunk ";"
  pure $ runPreInc ix r
runStm' (PostInc r) = do
  ix <- writeRef r
  _ <- addChunk "++;"
  pure $ runPostInc ix r
runStm' (PreDec r) = do
  _ <- addChunk "--"
  ix <- writeRef r
  _ <- addChunk ";"
  pure $ runPreDec ix r
runStm' (PostDec r) = do
  ix <- writeRef r
  _ <- addChunk "--;"
  pure $ runPostDec ix r
runStm' (WriteAdd r e) = do
  ixR <- writeRef r
  ixO <- addChunk " += "
  ixE <- writeExpr e
  _ <- addChunk ";"
  pure $ runWriteAdd ixR ixE r e
runStm' (WriteSub r e) = do
  ixR <- writeRef r
  ixO <- addChunk " -= "
  ixE <- writeExpr e
  _ <- addChunk ";"
  pure $ runWriteSub ixR ixE r e
runStm' (WriteMul r e) = do
  ixR <- writeRef r
  ixO <- addChunk " *= "
  ixE <- writeExpr e
  _ <- addChunk ";"
  pure $ runWriteMul ixR ixE r e
runStm' (Return e) = do
  _ <- addChunk "return "
  rIxs <- writeExpr e
  _ <- addChunk ";"
  pure $ runReturn rIxs e
