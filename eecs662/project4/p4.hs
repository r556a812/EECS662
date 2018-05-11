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

subst :: String -> FBAE -> FBAE -> FBAE
subst _ _ (Num x) = (Num x)
subst _ _ (Boolean x) = (Boolean x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Or l r) = (Or (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero x) = (IsZero (subst i v x))
subst i v (If c t f) = (If (subst i v c) (subst i v t) (subst i v f))
subst i v (Lambda i' t' b') = (Lambda i' t' (subst i v b'))
subst i v (App f' a') = (App (subst i v f') (subst i v a') )
subst i v (Fix f) = Fix (subst i v f)
subst i v (Bind i' v' b') = if i==i'
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))
subst i v (Id i') = if i==i'
                    then v
                    else (Id i')

-- Statically scoped eval

evalM :: Env -> FBAE -> (Maybe FBAEVal)
evalM env (Num x) = Just (NumV x)
evalM env (Boolean b) = Just (BooleanV b)
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
evalM env (Bind i v b) = do { v' <- (evalM env v);
                              evalM ((i,v'):env) b }
evalM env (Lambda i t b) = Just (ClosureV i t b env)
evalM env (App f a) = do { (ClosureV i t b e) <- (evalM env f);
                           a' <- (evalM env a);
                           (evalM ((i,a'):e) b) }
evalM env (Id i) = (lookup i env)
evalM env (And l r) = do { (BooleanV l') <- (evalM env l);
                           (BooleanV r') <- (evalM env r);
                           return (BooleanV (l'&&r')) }
evalM env (Or l r) = do { (BooleanV l') <- (evalM env l);
                          (BooleanV r') <- (evalM env r);
                          return (BooleanV (l'||r')) }
evalM env (Leq l r) = do { (NumV l') <- (evalM env l);
                           (NumV r') <- (evalM env r);
                           return (BooleanV (l'<=r')) }
evalM env (IsZero i) = do { (NumV i') <- (evalM env i);
                            return (BooleanV (i'==0)) }
evalM env (If c t e) = do { (BooleanV c') <- (evalM env c);
                            if c' then (evalM env t) else (evalM env e) }
evalM env (Fix f) = do { (ClosureV i t b e) <- (evalM env f);
                         (evalM e (subst i (Fix (Lambda i t b)) b)) }

-- Type inference function

type Cont = [(String,TFBAE)]

typeofM :: Cont -> FBAE -> (Maybe TFBAE)
typeofM _ (Num _) = Just TNum
typeofM _ (Boolean _) = Just TBool
typeofM c (Plus l r) = do { l' <- (typeofM c l);
                            r' <- (typeofM c r);
                            if l'==TNum && r'==TNum then return TNum else Nothing }
typeofM c (Minus l r) = do { l' <- (typeofM c l);
                             r' <- (typeofM c r);
                             if l'==TNum && r'==TNum then return TNum else Nothing }
typeofM c (Mult l r) = do { l' <- (typeofM c l);
                            r' <- (typeofM c r);
                            if l'==TNum && r'==TNum then return TNum else Nothing }
typeofM c (Div l r) = do { l' <- (typeofM c l);
                           r' <- (typeofM c r);
                           if l'==TNum && r'==TNum then return TNum else Nothing }
typeofM c (And l r) = do { l' <- (typeofM c l);
                           r' <- (typeofM c r);
                           if l'==TBool && r'==TBool then return TBool else Nothing }
typeofM c (Or l r) = do { l' <- (typeofM c l);
                          r' <- (typeofM c r);
                          if l'==TBool && r'==TBool then return TBool else Nothing }
typeofM c (Leq l r) = do { l' <- (typeofM c l);
                           r' <- (typeofM c r);
                           if l'==TNum && r'==TNum then return TBool else Nothing }
typeofM c (IsZero i) = do { i' <- (typeofM c i);
                            if i'==TNum then return TBool else Nothing }
typeofM c (If co t e) = do { c' <- (typeofM c co);
                             t' <- (typeofM c t);
                             e' <- (typeofM c e);
                             if c'==TBool && t'==e' then return t' else Nothing }
typeofM c (Bind i v b) = do { v' <- (typeofM c v);
                              (typeofM ((i,v'):c) b) }
typeofM c (Lambda i t b) = do { r <- (typeofM ((i,t):c) b);
                                return (t:->:r) }
typeofM c (App f a) = do { a' <- (typeofM c a);
                           (d:->:r) <- (typeofM c f);
                           if d==a' then return r else Nothing }
typeofM c (Id i) = lookup i c
typeofM c (Fix f) = do { (d:->:r) <- (typeofM c f);
                         return r }



-- Interpreter

interp :: FBAE -> (Maybe FBAEVal)
interp f = let env = [] in
            do { typeofM env f;
                 evalM env f }

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

testing = putStrLn $ show $ interp test1

main = do { testing }
