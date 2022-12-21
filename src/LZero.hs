{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
module LZero (Expr (Zero, True, False, Succ, Pred, IsZero, If), smallStep, eval)  where

import Prelude hiding (Bool, True, False)

data Bool
data Nat

data Expr a where
  Zero :: Expr Nat
  True :: Expr Bool
  False :: Expr Bool
  Succ :: Expr Nat -> Expr Nat
  Pred :: Expr Nat -> Expr Nat
  IsZero :: Expr Nat -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

deriving instance Show (Expr a)
deriving instance Eq (Expr a)

eval :: Expr a -> [String]
eval e = case inner e of
   Just ee -> show e : eval ee
   Nothing -> [show e]
  where inner = smallStep


smallStep :: Expr a -> Maybe (Expr a)
smallStep Zero = Nothing
smallStep True = Nothing
smallStep False = Nothing
smallStep e@(Succ _) = applySucc e
smallStep e@(Pred _) = applyPred e
smallStep e@(IsZero _) = applyIsZero e
smallStep e@If{}= applyIf e

applySucc :: Expr Nat -> Maybe (Expr Nat)
applySucc e = case smallStep e of
  Nothing -> Nothing
  Just stepped -> Just . Succ $ stepped

applyPred :: Expr Nat -> Maybe (Expr Nat)
applyPred (Pred Zero) = Just Zero
applyPred (Pred (Succ e)) = case smallStep e of 
  Nothing -> Just . Pred $ e
  Just stepped -> Just . Pred . Succ $ stepped
applyPred (Pred e) = case smallStep e of 
  Nothing -> Just . Pred $ e
  Just stepped -> Just . Pred $ stepped
applyPred _ = Nothing

applyIsZero :: Expr Bool -> Maybe (Expr Bool)
applyIsZero (IsZero Zero) = Just True
applyIsZero (IsZero e) = case smallStep e of
  Nothing -> Nothing
  Just stepped -> Just . IsZero $ stepped
applyIsZero _ = Nothing

applyIf :: Expr a -> Maybe (Expr a)
applyIf (If True eTrue _) = Just eTrue
applyIf (If False _ eFalse) = Just eFalse
applyIf (If eCond eTrue eFalse) = case smallStep eCond of
  Nothing -> Nothing
  Just stepped -> Just $ If stepped eTrue eFalse
applyIf _ = Nothing