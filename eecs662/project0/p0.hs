{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for QuickCheck
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

--
-- Simple caculator over naturals with no identifiers
--
-- Author: Perry Alexander
-- Date: Tue Jan 23 17:54:44 CST 2018
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--

-- AST Definition

data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- AE Parser (Requires ParserUtils and Parsec included above)

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

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]

numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)


term = parens lexer expr
       <|> numExpr
       <|> ifExpr

-- Parser invocation
-- Call parseAE to parse a string into the AE data structure.

parseAE = parseString expr

-- Evaluation Functions
-- Replace the bodies of these functions with your implementations for
-- Exercises 1-4.  Feel free to add utility functions or testing functions as
-- you see fit, but do not change the function signatures.  Note that only
-- Exercise 4 requires you to integrate the parser above.


--Author: Richard Aviles
--ID: 2823381

evalAE :: AE -> Int
evalAE (Num a) = a
evalAE (Plus l r) = (evalAE l) + (evalAE r)
evalAE (Minus l r) = if ( ((evalAE l) - (evalAE r)) < 0 )
			then error "!"
			else ((evalAE l) - (evalAE r))
evalAE (Mult l r) = (evalAE l) * (evalAE r)
evalAE (Div l r) = if ( (evalAE r) == 0 )
			then error "!"
			else (div (evalAE l) (evalAE r))
evalAE (If0 f s t) = if ( (evalAE f) == 0 )
			then (evalAE s)
			else (evalAE t)


evalAEMaybe :: AE -> Maybe Int
evalAEMaybe (Num a) = Just a
evalAEMaybe (Plus l r) = case (evalAEMaybe l) of
			  Just a -> case (evalAEMaybe r) of
				Just b -> Just (a+b)
				Nothing -> Nothing
			  Nothing -> Nothing
evalAEMaybe (Minus l r) = case (evalAEMaybe l) of
			   Just a -> case (evalAEMaybe r) of
				Just b -> if (a-b < 0) then Nothing else Just (a-b)
				Nothing -> Nothing
			   Nothing -> Nothing
evalAEMaybe (Mult l r) = case (evalAEMaybe l) of
			  Just a -> case (evalAEMaybe r) of
				Just b -> Just (a*b)
				Nothing -> Nothing
			  Nothing -> Nothing
evalAEMaybe (Div l r) = case (evalAEMaybe l) of
			 Just a -> case (evalAEMaybe r) of
				Just b -> if (b == 0) then Nothing else Just (div a b)
				Nothing -> Nothing
			 Nothing -> Nothing
evalAEMaybe (If0 f s t) = case (evalAEMaybe f) of
			   Just a -> if (a == 0) then (evalAEMaybe s) else (evalAEMaybe t)
			   Nothing -> Nothing

evalM :: AE -> Maybe Int
evalM (Num a) = Just a
evalM (Plus l r) = do { v1 <- (evalM l);
			v2 <- (evalM r);
			return (v1+v2) }
evalM (Minus l r) = do {v1 <- (evalM l);
			v2 <- (evalM r);
			if (v1-v2 < 0) then Nothing else return (v1-v2) }
evalM (Mult l r) = do { v1 <- (evalM l);
			v2 <- (evalM r);
			return (v1*v2) }
evalM (Div l r) = do  { v1 <- (evalM l);
			v2 <- (evalM r);
			if (v2 == 0) then Nothing else return (div v1 v2) }
evalM (If0 f s t) = do {v1 <- (evalM f);
			if (v1 == 0) then (evalM s) else (evalM t) }

interpAE :: String -> Maybe Int
interpAE = evalM . parseAE

-- Change the eval to whatever I want to test
-- I did test all three and all three work
test1 :: IO()
test1 = putStrLn $ show $ evalAE (Num 1)

test2 :: IO()
test2 = putStrLn $ show $ evalAE (Plus(Num 10) (Num 5))

test3 :: IO()
test3 = putStrLn $ show $ evalAE (Minus (Num 10) (Num 5))

test4 :: IO()
test4 = putStrLn $ show $ evalAE (Minus (Num 5) (Num 10))

test5 :: IO()
test5 = putStrLn $ show $ evalAE (Mult (Num 10) (Num 5))

test6 :: IO()
test6 = putStrLn $ show $ evalAE (Div (Num 10) (Num 5))

test7 :: IO()
test7 = putStrLn $ show $ evalAE (If0 (Num 0) (Num 5) (Num 10))

test8 :: IO()
test8 = putStrLn $ show $ evalAE (If0 (Num 1) (Num 5) (Num 10))


--Test4 is commented out to test everyting for evalAE
--Take out the comment to run Test4
main = do { test1;
            test2;
            test3;
            --test4;
            test5;
            test6;
            test7;
            test8}
