{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- AST Definition

data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show,Eq)

data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Mult :: ABE -> ABE -> ABE
  Div :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  deriving (Show,Eq)

-- AST Pretty Printer

pprint :: ABE -> String
pprint (Num n) = show n
pprint (Boolean b) = show b
pprint (Plus n m) = "(" ++ pprint n ++ " + " ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ " - " ++ pprint m ++ ")"
pprint (Mult n m) = "(" ++ pprint n ++ " * " ++ pprint m ++ ")"
pprint (Div n m) = "(" ++ pprint n ++ " / " ++ pprint m ++ ")"
pprint (And n m) = "(" ++ pprint n ++ " && " ++ pprint m ++ ")"
pprint (Leq n m) = "(" ++ pprint n ++ " <= " ++ pprint m ++ ")"
pprint (IsZero m) = "(isZero " ++ pprint m ++ ")"
pprint (If c n m) = "(if " ++ pprint c ++ " then " ++ pprint n ++ " else " ++ pprint m ++ ")"


-- Parser (Requires ParserUtils and Parsec)

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "if0"
                              , "then"
                              , "else"
                              ]
            , reservedOpNames = [ "+","-","*","/"]
            }
  
lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser ABE
expr = buildExpressionParser opTable term

opTable = [ [ inFix "*" Plus AssocLeft
            , inFix "/" Minus AssocLeft ]
          , [ inFix "+" Plus AssocLeft
            , inFix "-" Minus AssocLeft ]
          , [ inFix "<=" Leq AssocLeft
            , preFix "isZero" IsZero ]
          , [ inFix "&&" And AssocLeft ]
          ]

numExpr :: Parser ABE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

trueExpr :: Parser ABE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser ABE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser ABE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

term = parens lexer expr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr

-- Parser invocation

parseABE = parseString expr

-- Evaluation Functions

liftNum :: (Int -> Int -> Int) -> ABE -> ABE -> ABE
liftNum f (Num l) (Num r) = (Num (f l r))

liftBool :: (Bool -> Bool -> Bool) -> ABE -> ABE -> ABE
liftBool f (Boolean l) (Boolean r) = (Boolean (f l r))

liftNum2Bool :: (Int -> Int -> Bool) -> ABE -> ABE -> ABE
liftNum2Bool f (Num l) (Num r) = (Boolean (f l r))


evalM :: ABE -> (Maybe ABE)
evalM (Num a) = Just (Num a)
evalM (Boolean b) = Just (Boolean b)
evalM (Plus l r) = do { v1 <- (evalM l);
			v2 <- (evalM r);
			return (liftNum (+) v1 v2) }
evalM (Minus l r) = do {v1 <- (evalM l);
			v2 <- (evalM r);
			--if ((liftNum (-) v1 v2) < 0) then Nothing else return (liftNum (-) v1 v2) }
			return (liftNum (-) v1 v2) }
evalM (Mult l r) = do { v1 <- (evalM l);
			v2 <- (evalM r);
			return (liftNum (*) v1 v2) }
evalM (Div l r) = do  { v1 <- (evalM l);
			v2 <- (evalM r);
			if (v2 == (Num 0)) then Nothing else return (liftNum (div) v1 v2) }
evalM (Leq l r) = do { v1 <- (evalM l);
			v2 <- (evalM r);
			--if (liftNum2Bool (<) v1<v2 ) then return (Boolean True) else return (Boolean False) }
			return (Boolean True) }
evalM (IsZero n) = do { v1 <- (evalM n);
			if v1==(Num 0) then return (Boolean True) else return (Boolean False) }
evalM (If c t e) = do { v1 <- (evalM c);
			if v1==(Boolean True) then (evalM t) else (evalM e) }

evalErr :: ABE -> (Maybe ABE)
evalErr (Num a) = Just (Num a)
evalErr (Boolean b) = Just (Boolean b)
evalErr (Plus l r) = do { (Num x) <- (evalErr l);
			(Num y) <- (evalErr r);
			return (Num (x+y)) }
evalErr (Minus l r) = do { (Num x) <- (evalErr l);
			 (Num y) <- (evalErr r);
			 if (x-y) < 0 then Nothing else return (Num (x-y)) }
evalErr (Mult l r) = do { (Num x) <- (evalErr l);
			(Num y) <- (evalErr r);
			return (Num (x*y)) }
evalErr (Div l r) = do { (Num x) <- (evalErr l);
			(Num y) <- (evalErr r);
			if (y==0) then Nothing else return (Num (div x y)) }
evalErr (And l r) = do { (Boolean x) <- (evalErr l);
			(Boolean y) <- (evalErr r);
			return (Boolean (x&&y)) }
evalErr (Leq l r) = do { (Num x) <- (evalErr l);
			(Num y) <- (evalErr r);
			return (Boolean (x<=y)) }
evalErr (IsZero n) = do { (Num x) <- (evalErr n);
			if x==0 then return (Boolean True) else return (Boolean False) }
evalErr (If c t e) = do { (Boolean b) <- (evalErr c);
			if b then (evalErr t) else (evalErr e) }

-- Type Derivation Function

typeofM :: ABE -> Maybe TABE
typeofM (Num x) = Just TNum
typeofM (Boolean b) = Just TBool
typeofM (Plus l r) = do { x <- (typeofM l);
			  y <- (typeofM r);
			  if x==TNum && y==TNum then return TNum else Nothing }
typeofM (Minus l r) = do { x <- (typeofM l);
			   y <- (typeofM r);
			   if x==TNum && y==TNum then return TNum else Nothing }
typeofM (Mult l r) = do { x <- (typeofM l);
			  y <- (typeofM r);
			  if x==TNum && y==TNum then return TNum else Nothing }
typeofM (Div l r) = do { x <- (typeofM l);
			 y <- (typeofM r);
			 if x==TNum && y==TNum then return TNum else Nothing }
typeofM (And l r) = do { x <- (typeofM l);
			 y <- (typeofM r);
			 if x==TBool && y==TBool then return TBool else Nothing }
typeofM (Leq l r) = do { x <- (typeofM l);
			 y <- (typeofM r);
			 if x==TNum && y==TNum then return TBool else Nothing }
typeofM (IsZero n) = do { x <- (typeofM n);
			  if x==TNum then return TNum else Nothing }
typeofM (If c t e) = do { x <- (typeofM c);
			  y <- (typeofM t);
			  z <- (typeofM e);
			  if x==TBool && y==z then return y else Nothing }


-- Combined interpreter

evalTypeM e = do { t <- typeofM e; 
		   evalM e; }

-- Optimizer

optimize :: ABE -> ABE
optimize (Num a) = Num a
optimize (Boolean b) = Boolean b
optimize (Plus l (Num 0)) = (optimize l)
optimize (Plus l r) = Plus (optimize l) (optimize r)
optimize (Minus l r) = Minus (optimize l) (optimize r)
optimize (Mult l r) = Mult (optimize l) (optimize r)
optimize (Div l r) = Div (optimize l) (optimize r)
optimize (And l r) = And (optimize l) (optimize r)
optimize (Leq l r) = Leq (optimize l) (optimize r)
optimize (IsZero n) = IsZero (optimize n)
optimize (If (Boolean True) t e) = (optimize t)
optimize (If (Boolean False) t e) = (optimize e)

interpOptM :: ABE -> Maybe ABE
interpOptM a = evalM (optimize a)


testlist = [ (Num 1), (Plus (Num 1) (Num 2)), (Minus (Num 2) (Num 1)), (Mult (Num 2) (Num 3)), (Div (Num 6) (Num 3)), (IsZero (Num 0)), (If (Boolean True) (Num 1) (Num 2)), (If (Boolean True) (Num 1) (Num 2)) ]

test1 :: IO()
test1 = putStrLn $ show $ map evalM testlist

test2 :: IO()
test2 = putStrLn $ show $ map evalErr testlist


test3 :: IO()
test3 = putStrLn $ show $ map typeofM testlist


test4 :: IO()
test4 = putStrLn $ show $ map evalTypeM testlist


test5 :: IO()
test5 = putStrLn $ show $ map optimize testlist


main = do { test1;
	    test2;
	    test2;
	    test3;
	    test4;
	    test5 }













