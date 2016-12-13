module Expr where

import Parsing
import Data.Char
import Data.Maybe
import Data.List
import Data.String
import System.Random
import Control.Monad (replicateM)
import Test.QuickCheck


data Expr = Lit Double
    | Add Expr Expr
    | Mul Expr Expr
    | Var 
    | Sin Expr
    | Cos Expr
    deriving (Eq)

instance Show Expr where
  show = showExpr

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add a b) = showExpr a ++ " + " ++ showExpr b
showExpr (Mul a b) = showFactor a ++ " * " ++ showFactor b
showExpr Var = "x"
showExpr (Sin a) = "sin " ++ showBrackets a 
showExpr (Cos a) = "cos " ++ showBrackets a 

showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")" 
showFactor e = showExpr e

showBrackets :: Expr -> String
showBrackets (Add a b) = "(" ++ showExpr (Add a b) ++ ")" 
showBrackets (Mul a b) = "(" ++ showExpr (Mul a b) ++ ")" 
showBrackets e = showExpr e 


eval :: Expr -> Double -> Double
eval (Lit n) _ = n
eval (Add a b) x = eval a x + eval b x
eval (Mul a b) x = eval a x * eval b x
eval Var x = x
eval (Sin a) x = sin (eval a x)
eval (Cos a) x = cos (eval a x)  

readExpr :: String -> Maybe Expr              
readExpr s = let s' = filter (not.isSpace) s
             in case parse expr s' of
                     Just (e,"") -> Just e
                     _           -> Nothing

leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = do is <- chain item sep
                           return (foldl1 op is)

expr, term, factor :: Parser Expr

expr = leftAssoc Add term (char '+')

term = leftAssoc Mul factor (char '*')

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

sinParse = do   s <- string "sin"
                return Sin  

cosParse = do   s <- string "cos"
                return Cos  

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr ex = showExpr (fromJust (readExpr $ showExpr ex)) == showExpr ex

simplify :: Expr -> Expr
simplify (Add a b) = simplifiedAdd (simplify a) (simplify b)
simplify (Mul a b) = simplifiedMul (simplify a) (simplify b)
simplify (Sin a) = simplifiedSin (simplify a)
simplify (Cos a) = simplifiedCos (simplify a) 
simplify e = e 

simplifiedAdd :: Expr -> Expr -> Expr
simplifiedAdd (Lit 0) e = e
simplifiedAdd e (Lit 0) = e
simplifiedAdd (Lit n) (Lit m) = Lit (n+m)
simplifiedAdd Var Var = Mul (Lit 2) Var
simplifiedAdd (Mul (Lit n) e1) e2 | e1 == e2 = Mul (Lit (n+1)) e1
simplifiedAdd (Mul (Lit n) e1) (Mul (Lit m) e2) | e1 == e2 = Mul (Lit (n+m)) e1
simplifiedAdd e1 e2 = Add e1 e2

simplifiedMul :: Expr -> Expr -> Expr
simplifiedMul (Lit 0) _ =  Lit 0
simplifiedMul _ (Lit 0) = Lit 0
simplifiedMul (Lit 1) e = e
simplifiedMul e (Lit 1) = e
simplifiedMul (Lit n) (Lit m) = Lit (n*m)
simplifiedMul (Mul (Lit n) e1) e2 | e1 == e2 = Mul (Lit n) (Mul e1 e2)
simplifiedMul (Mul (Lit n) e1) (Mul (Lit m) e2) | e1 == e2 = Mul (Lit (n*m)) (Mul e1 e2)
simplifiedMul e1 (Lit n) = Mul (Lit n) e1
simplifiedMul e1 e2 = Mul e1 e2

simplifiedSin :: Expr -> Expr
simplifiedSin (Lit n) = Lit (sin n)
simplifiedSin e = Sin e

simplifiedCos :: Expr -> Expr
simplifiedCos (Lit n) = Lit (cos n)
simplifiedCos e = Cos e

prop_simplify :: Expr -> Double -> Bool
prop_simplify e d = eval e d == eval (simplify e) d

-- TODO add property for checking if it simplifies enough.

differentiate :: Expr -> Expr
differentiate e = simplify $ differentiate' (simplify e)
    where
        differentiate' (Lit _) = Lit 0
        differentiate' Var = Lit 1
        differentiate' (Sin e) = Mul (differentiate' e) (Cos e)
        differentiate' (Cos e) = Mul (Mul (Lit (-1)) (differentiate' e)) (Sin e)
        differentiate' (Add a b) = Add (differentiate' a) (differentiate' b)
        differentiate' (Mul a b) = Add (Mul (differentiate' a) b) (Mul a (differentiate' b))

