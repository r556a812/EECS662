{-# LANGUAGE GADTs #-}

import Text.ParserCombinators.Parsec
import Control.Monad
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Calculator language extended with an environment to hold defined variables

data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show,Eq)

data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Mult :: FBAE -> FBAE -> FBAE
  Div :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> TFBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  Boolean :: Bool -> FBAE
  And :: FBAE -> FBAE -> FBAE
  Or :: FBAE -> FBAE -> FBAE
  Leq :: FBAE -> FBAE -> FBAE
  IsZero :: FBAE -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  Fix :: FBAE -> FBAE
  deriving (Show,Eq)

-- Value defintion for statically scoped eval

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> TFBAE -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)

-- Enviornment for statically scoped eval

type Env = [(String,FBAEVal)]

-- Statically scoped eval

evalM :: Env -> FBAE -> (Maybe FBAEVal)
evalM env (Num x) = Just (NumV x)
evalM env (Plus l r) = do { (NumV l') <- (evalM env l);
                            (NumV r') <- (evalM env r);
                            return (NumV (l'+r')) }
evalM env (Minus l r) = do { (NumV l') <- (evalM env l);
                             (NumV r') <- (evalM env r);
                             return (NumV (l'-r')) }
evalM env (Mult l r) = do { (NumV l') <- (evalM env l);
                            (NumV r') <- (evalM env r);
                            return (NumV (l'*r')) }
evalM env (Div l r) = do { (NumV l') <- (evalM env l);
                           (NumV r') <- (evalM env r);
                           return (NumV (div l' r')) }
evalM env (Bind i v b) = evalM env (App (Lambda i b) v)
evalM env (Lambda i t b) = Just (ClosureV i b env)
evalM env (App f a) = do { (ClosureV i b e) <- (evalM env f);
                           a' <- (evalM env a);
                           (evalM ((i,a'):e) b) }
evalM env (Id i) = (lookup i env)
evalM env (Boolean b) = Just (BooleanV b)
evalM env (And l r) = do { (BooleanV l') <- (evalM env l);
                           (BooleanV r') <- (evalM env r);
                           return (BooleanV (l'&&r')) }
evalM env (Or l r) = do { (BooleanV l') <- (evalM env l);
                          (BooleanV r') <- (evalM env r);
                          return (BooleanV (l'||r')) }
evalm env (Leq l r) = do { (NumV l') <- (evalM env l);
                           (NumV r') <- (evalM env r);
                           return (BooleanV (l'<=r')) }
evalM env (IsZero x) = do { (NumV x') <- (evalM env x);
                            return (BooleanV (x'==(NumV 0))) }
evalM env (If c t e) = do { (BooleanV c') <- (evalM env c);
                            if c then (evalM env t) else (evalM env e) }
evalM env (Fix f) = do { (ClosureV i b e) <- (evalM env f);
                         (evalM env (subst i (Fix (Lambda i b)) b)) }

-- Type inference function

type Cont = [(String,TFBAE)]

typeofM :: Cont -> FBAE -> (Maybe TFBAE)
typeofM _ _ = Nothing


-- Interpreter

interp :: FBAE -> (Maybe FBAEVal)
interp _ = Nothing

-- Factorial function for testing evalM and typeofM.  the type of test1 should
-- be TNum and the result of evaluating test1`should be (NumV 6).  Remember
-- that Just is used to return both the value and type.

test1 = (Bind "f" (Lambda "g" ((:->:) TNum TNum)
                    (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1)
                                         (Mult (Id "x")
                                               (App (Id "g")
                                                    (Minus (Id "x")
                                                           (Num 1)))))))
         (App (Fix (Id "f")) (Num 3)))

ee = []

test13 :: IO()
test13 = putStrLn $ show $ map (evalM ee) test1

main = do { test13 }
