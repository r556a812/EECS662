{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- BBAE AST and Type Definitions

data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  deriving (Show,Eq)

type Env = [(String,BBAE)]

type Cont = [(String,TBBAE)]

evalS :: BBAE -> (Maybe BBAE)
evalS e (Num x) = Just(Num x)
evalS e (Plus l r) = do { (Num x) <- evalS e l;
			(Num y) <- evalS e r;
			return (Num (x+y)) }
evalS e (Minus l r) = do { (Num x) <- evalS e l;
			 (Num y) <- evalS e r;
			 return (Num (x+y)) }
evalS e (Bind i v b) = do { v' <- evalS e v; 
			    evalS (i,v'):e b }
evalS e (Id n) = (lookup n e)
evalS e (Boolean b) = Just(Boolean b)
evalS e (And l r) = do { (Boolean x) <- evalS e l;
			 (Boolean y) <- evalS e r;
			 return (Boolean (x&&y)) } 
evalS e (Leq l r) = do { (Num x) <- evalS e l;
			 (Num y) <- evalS e r;
			 return (Boolean (x<=y)) }
evalS e (IsZero n) = do { (Num x) <- evalS e n;
			  if x==0 then return (Boolean True) else return (Boolean False) }
evalS e (If c t e) 

evalM :: Env -> BBAE -> (Maybe BBAE)
evalM _ _ = Nothing

testBBAE :: BBAE -> Bool
testBBAE _ = True

typeofM :: Cont -> BBAE -> (Maybe TBBAE)
typeofM _ _ = Nothing

evalT :: BBAE -> (Maybe BBAE)
evalT _ = Nothing

