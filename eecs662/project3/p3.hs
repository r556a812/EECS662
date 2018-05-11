{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- CFAE AST and Type Definitions

data CFAE where
  Num :: Int -> CFAE
  Plus :: CFAE -> CFAE -> CFAE
  Minus :: CFAE -> CFAE -> CFAE
  Lambda :: String -> CFAE -> CFAE
  App :: CFAE -> CFAE -> CFAE
  Id :: String -> CFAE
  If0 :: CFAE -> CFAE -> CFAE -> CFAE
  deriving (Show,Eq)

type Env = [(String,CFAE)]

evalDynCFAE :: Env -> CFAE -> (Maybe CFAE)
evalDynCFAE env (Num x) = Just (Num x)
evalDynCFAE env (Plus l r) = do { (Num l') <- evalDynCFAE env l;
                                (Num r') <- evalDynCFAE env r;
                                return (Num (l'+r')) }
evalDynCFAE env (Minus l r) = do { (Num l') <- evalDynCFAE env l;
                                 (Num r') <- evalDynCFAE env r;
                                 return (Num (l'-r')) }
evalDynCFAE env (Lambda i b) = Just (Lambda i b)
evalDynCFAE env (App f a) = do { (Lambda i b) <- evalDynCFAE env f;
                                 a' <- evalDynCFAE env a;
                                 (evalDynCFAE ((i,a'):env) b) }
evalDynCFAE env (Id i) = lookup i env
evalDynCFAE env (If0 c t e) = do { (Num c') <- evalDynCFAE env c;
                                   if c'== 0 then evalDynCFAE env t else evalDynCFAE env e }


data CFAEValue where
  NumV :: Int -> CFAEValue
  ClosureV :: String -> CFAE -> Env' -> CFAEValue
  deriving (Show,Eq)

type Env' = [(String,CFAEValue)]

evalStatCFAE :: Env' -> CFAE -> (Maybe CFAEValue)
evalStatCFAE env (Num x) = Just (NumV x)
evalStatCFAE env (Plus l r) = do { (NumV l') <- (evalStatCFAE env l);
                                   (NumV r') <- (evalStatCFAE env r);
                                   return (NumV (l'+r')) }
evalStatCFAE env (Minus l r) = do { (NumV l') <- (evalStatCFAE env l);
                                    (NumV r') <- (evalStatCFAE env r);
                                    return (NumV (l'+r')) }
evalStatCFAE env (Lambda i b) = Just (ClosureV i b env)
evalStatCFAE env (App f a) = do { (ClosureV i b e) <- (evalStatCFAE env f);
                                  a' <- (evalStatCFAE env a);
                                  (evalStatCFAE ((i,a'):e) b) }
evalStatCFAE env (Id i) = (lookup i env)
evalStatCFAE env (If0 c t e) = do { (NumV c') <- (evalStatCFAE env c);
                                    if (NumV c') == (NumV 0) then (evalStatCFAE env t)  else (evalStatCFAE env e) }

data CFBAE where
  Num' :: Int -> CFBAE
  Plus' :: CFBAE -> CFBAE -> CFBAE
  Minus' :: CFBAE -> CFBAE -> CFBAE
  Lambda' :: String -> CFBAE -> CFBAE
  App' :: CFBAE -> CFBAE -> CFBAE
  Bind' :: String -> CFBAE -> CFBAE -> CFBAE
  Id' :: String -> CFBAE
  If0' :: CFBAE -> CFBAE -> CFBAE -> CFBAE
  deriving (Show,Eq)

elabCFBAE :: CFBAE -> CFAE
elabCFBAE (Num' x) = (Num x)
elabCFBAE (Plus' l r) = (Plus (elabCFBAE l) (elabCFBAE r))
elabCFBAE (Minus' l r) = (Minus (elabCFBAE l) (elabCFBAE r))
elabCFBAE (Lambda' i b) = let b' = (elabCFBAE b)
                          in (Lambda i b')
elabCFBAE (App' f a) = let f' = (elabCFBAE f)
                           a' = (elabCFBAE a)
                       in (App f' a')
elabCFBAE (Bind' i v b) = let v' = (elabCFBAE v)
                              b' = (elabCFBAE b)
                          in (App (Lambda i b') v')
elabCFBAE (Id' i) = (Id i)
elabCFBAE (If0' c t e) = let c' = (elabCFBAE c)
                             t' = (elabCFBAE t)
                             e' = (elabCFBAE e)
                         in (If0 c' t' e')


evalCFBAE :: Env' -> CFBAE -> (Maybe CFAEValue)
evalCFBAE env x = (evalStatCFAE env (elabCFBAE x))


-- Tests for evalDynCFAE and evalDynCFAE.  test2 and test3 should demonstrate
-- the difference between static and dynamic scoping.  If you get the same
-- results with both interpreters, you've got problems.

test0=(App (Lambda "inc" (Id "inc")) (Lambda "x" (Plus (Id "x") (Num 1))))
test1=(App (Lambda "inc" (App (Id "inc") (Num 3))) (Lambda "x" (Plus (Id "x") (Num 1))))
test2=(App (Lambda "n" (App (Lambda "inc" (App (Lambda "n" (App (Id "inc") (Num 3))) (Num 3))) (Lambda "x" (Plus (Id "x") (Id "n"))))) (Num 1))
test3=(App (Lambda "Sum" (App (Id "Sum") (Num 3))) (Lambda "x" (If0 (Id "x") (Num 0) (Plus (Id "x") (App (Id "Sum") (Minus (Id "x") (Num 1)))))))

-- List of tests if you would like to use map for testing

tests = [test0,test1,test2,test3]
ee = []
ee2 = []

test11 :: IO()
test11 = putStrLn $ show $ map (evalDynCFAE ee) tests

test12 :: IO()
test12 = putStrLn $ show $ map (evalStatCFAE ee2) tests

-- Tests for evalCFBAE and evalDynCFAE.  These are the same tests as above
-- using Bind.  You should get the same results from evalCFBAE that you
-- get from evalStateCFAE.

test0'= (Bind' "inc" (Lambda' "x" (Plus' (Id' "x") (Num' 1))) (Id' "inc"))
test1' = (Bind' "inc" (Lambda' "x" (Plus' (Id' "x") (Num' 1))) (App' (Id' "inc") (Num' 3)))
test2' = (Bind' "n" (Num' 1) (Bind' "inc" (Lambda' "x" (Plus' (Id' "x") (Id' "n"))) (Bind' "n" (Num' 3) (App' (Id' "inc") (Num' 3)))))
test3' = (Bind' "Sum" (Lambda' "x" (If0' (Id' "x") (Num' 0) (Plus' (Id' "x") (App' (Id' "Sum") (Minus' (Id' "x") (Num' 1)))))) (App' (Id' "Sum") (Num' 3)))

-- List of tests if you would like to use map for testing

tests' = [test0',test1',test2',test3']
ee3 = []
ee4 = []
ee5 = []

test13 :: IO()
test13 = putStrLn $ show $ map (evalCFBAE ee3) tests'



main = do { test11;
            test12;
            test13 }
