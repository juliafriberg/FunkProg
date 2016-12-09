
import Parsing
import Data.Char
import Data.Maybe
import Data.List
import System.Random
import Test.QuickCheck.Gen


data Expr = Lit Double
    | Add Expr Expr
    | Mul Expr Expr
    | Var 
    | Sin Expr
    | Cos Expr
    deriving (Show, Eq)

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add a b) = showExpr a ++ " + " ++ showExpr b
showExpr (Mul a b) = showFactor a ++ " * " ++ showFactor b
showExpr (Var) = "x"
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
eval (Var) x = x
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

factor = (Lit <$> number) 
        <|> var 
        <|> sinParse <*> factor 
        <|> cosParse <*> factor 
        <|> (char '(' *> expr <* char ')')

-- | Parse a number
number :: Parser Double
number = do 
    s <- (chain (oneOrMore digit) (char '.'))
    if length s < 3 then return (read (intercalate "." s)) else failure 

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
prop_ShowReadExpr ex = fromJust (readExpr $ showExpr ex) == ex


arbExpr :: Int -> Gen Expr
arbExpr n 
    | n == 0 = rNum
    | otherwise = oneof [rAdd, rMul, rVar, rSin, rCos, rNum]
    where
        rNum = do
            a <- choose(0,1000)
            b <- (fromInteger $ choose(0,9)) `div` 10 
            return $ Lit (a+b) 
        rAdd = undefined
        rMul = undefined 
        rVar = undefined
        rSin = undefined
        rCos = undefined





