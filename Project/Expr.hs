module Expr where

import Parsing
import Data.Char
import Data.Maybe
import Data.List
import Data.String
import System.Random
import Control.Monad (replicateM)
import Test.QuickCheck

-- The recursive datatype for Expr, which consists of 
-- floating point numbers, operands, variables and functions.
data Expr = Lit Double
    | Op Operator Expr Expr
    | Var 
    | F Function Expr
    deriving (Eq)

instance Show Expr where
  show = showExpr

-- The datatype for functions. Either Sin or Cos.
data Function = Sin | Cos
    deriving (Eq, Show)

-- The datatype for operands. Either Add or Mul.
data Operator = Mul | Add
    deriving (Eq, Show)

-- Takes a Function and returns the corresponding math function.
getFun :: Floating a => Function -> (a -> a)
getFun Sin = sin
getFun Cos = cos

-- Converts an expression to a string.
showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Op Add a b) = showExpr a ++ " + " ++ showExpr b
showExpr (Op Mul a b) = showFactor a ++ " * " ++ showFactor b
    where 
        -- To make sure parantheses are where they should be.
        showFactor :: Expr -> String
        showFactor (Op Add a b) = "(" ++ showExpr (Op Add a b) ++ ")" 
        showFactor e = showExpr e
showExpr Var = "x"
showExpr (F f a) = map toLower (show f) ++ " " ++ showBrackets a
    where
        -- To make sure parantheses are where they should be.
        showBrackets :: Expr -> String
        showBrackets (Op ops a b) = "(" ++ showExpr (Op ops a b) ++ ")" 
        showBrackets e = showExpr e 

-- Calculates the value of an expression.
eval :: Expr -> Double -> Double
eval (Lit n) _ = n
eval (Op Add a b) x = eval a x + eval b x
eval (Op Mul a b) x = eval a x * eval b x
eval Var x = x
eval (F f a) x = getFun f (eval a x)

-- Interprets the string as an expression. 
-- Returns Just expression if succeeds, otherwise Nothing. 
readExpr :: String -> Maybe Expr              
readExpr s = let s' = filter (not.isSpace) s
             in case parse expr s' of
                     Just (e,"") -> Just e
                     _           -> Nothing

-- Parse a list of items with separators
-- Taken from the lecture in week 4 about parsing.
leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = do is <- chain item sep
                           return (foldl1 op is)

-- Parsers for expr, term and factor.
-- expr and term are taken from the lecture in week 4 about parsing. 
-- factor has been changed by adding parsers for variables and functions.
expr, term, factor :: Parser Expr

expr = leftAssoc (Op Add) term (char '+')

term = leftAssoc (Op Mul) factor (char '*')

factor = (Lit <$> num) 
        <|> var 
        <|> sinParse <*> factor 
        <|> cosParse <*> factor 
        <|> (char '(' *> expr <* char ')')

-- | Parse a number
num = readsP ::  Parser Double

-- | Parse a variable
var :: Parser Expr
var = do s <- char 'x'
         return Var

-- Parser for strings
string :: String -> Parser String
string ""       = return ""
string (c:s)    = do 
                    c' <- char c
                    s' <- string s
                    return (c':s')  
-- Parser for sin
sinParse = do   s <- string "sin"
                return (F Sin)  

-- Parser for cos
cosParse = do   s <- string "cos"
                return (F Cos)  

-- Simplifies an expression. 
-- Simplifies x + 0, 0 * x, 1 * x and similar terms.
-- Simplifies subexpresssions not involving variables to 
-- their smallest representation. 
-- Simplifies 2 * x + 4 * x to 6 * x and similar expressions.
-- Simplifies (2 * x) * (4 * x) to 8 * x * x an similar expressions.
simplify :: Expr -> Expr
simplify (Op Add a b) = simplifiedAdd (simplify a) (simplify b)
    where
        -- Simplifies addition expressions
        simplifiedAdd :: Expr -> Expr -> Expr
        simplifiedAdd (Lit 0) e = e
        simplifiedAdd e (Lit 0) = e
        simplifiedAdd (Lit n) (Lit m) = Lit (n+m)
        simplifiedAdd Var Var = Op Mul (Lit 2) Var
        simplifiedAdd (Op Mul (Lit n) e1) e2 
                | e1 == e2 = Op Mul (Lit (n+1)) e1
        simplifiedAdd (Op Mul (Lit n) e1) (Op Mul (Lit m) e2) 
                | e1 == e2 = Op Mul (Lit (n+m)) e1
        simplifiedAdd e1 e2 = Op Add e1 e2

simplify (Op Mul a b) = simplifiedMul (simplify a) (simplify b)
    where 
        -- Simplifies multiplication expressions
        simplifiedMul :: Expr -> Expr -> Expr
        simplifiedMul (Lit 0) _ =  Lit 0
        simplifiedMul _ (Lit 0) = Lit 0
        simplifiedMul (Lit 1) e = e
        simplifiedMul e (Lit 1) = e
        simplifiedMul (Lit n) (Lit m) = Lit (n*m)
        simplifiedMul (Op Mul (Lit n) e1) e2 
                | e1 == e2 = Op Mul (Lit n) (Op Mul e1 e2)
        simplifiedMul (Op Mul (Lit n) e1) (Op Mul (Lit m) e2) 
                | e1 == e2 = Op Mul (Lit (n*m)) (Op Mul e1 e2)
        simplifiedMul e1 (Lit n) = Op Mul (Lit n) e1
        simplifiedMul e1 e2 = Op Mul e1 e2

simplify (F f a) = simplifiedFun (F f (simplify a))
    where
        -- Simplifies function expressions
        simplifiedFun :: Expr -> Expr
        simplifiedFun (F f (Lit n)) = Lit (getFun f n)
        simplifiedFun (F f a) = F f a

simplify e = e 

-- Differentiates the expression with respect to x
differentiate :: Expr -> Expr
differentiate e = simplify $ diff (simplify e)
    where
        -- Rules for calculating derivatives
        diff (Lit _) = Lit 0
        diff Var = Lit 1
        diff (F Sin e) = Op Mul (diff e) (F Cos e)
        diff (F Cos e) = 
            Op Mul (Op Mul (Lit (-1)) (diff e)) (F Sin e)
        diff (Op Add a b) = 
            Op Add (diff a) (diff b)
        diff (Op Mul a b) = 
            Op Add (Op Mul (diff a) b) (Op Mul a (diff b))

