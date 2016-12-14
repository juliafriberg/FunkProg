module ExprQC where

import Test.QuickCheck
import Expr
import Data.List
import Data.Maybe

-- A generator for expressions. 
arbExpr :: Int -> Gen Expr
arbExpr n 
    | n == 0 = oneof [return Var, rNum]
    | otherwise = oneof [rOp, return Var, rFun, rNum]
    where
        rNum = do
            a <- elements [1..10 :: Double]
            b <- elements [1..10 :: Double] 
            return $ Lit (a + b / 10.0)  
        rOp = do
            operand <- oneof [return Add, return Mul]
            term1 <- arbExpr size
            term2 <- arbExpr size
            return (Op operand term1 term2)
        rFun = do
            fun <- oneof [return Sin, return Cos]
            expr <- arbExpr (n-1)
            return (F fun expr)
        size = n `div` 2

instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- Tests that readExpr and showExpr works.
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr ex = showExpr (fromJust (readExpr $ showExpr ex)) == showExpr ex

-- Tests that the simplified version has the same value 
-- as the original expression and that it has been simplified 
-- as much as expected.
prop_simplify :: Expr -> Double -> Bool
prop_simplify e d = (eval e d == eval simplifiedExpr d) && isSimplified simplifiedExpr
    where 
        simplifiedExpr = simplify e
        isSimplified :: Expr -> Bool
        isSimplified (F f (Lit _)) = False
        isSimplified (F f e) = isSimplified e
        isSimplified (Op Add Var Var) = False
        isSimplified (Op Mul (Lit 1) e) = False
        isSimplified (Op ops (Lit 0) e) = False
        isSimplified (Op ops e (Lit 0)) = False
        isSimplified (Op ops (Lit _) (Lit _)) = False
        isSimplified (Op ops (Op Mul (Lit n) e1) e2) 
                | e1 == e2 = False
        isSimplified (Op ops (Op Mul (Lit n) e1) (Op Mul (Lit m) e2))
                | e1 == e2 = False
        isSimplified (Op ops e1 e2) = isSimplified e1 && isSimplified e2
        isSimplified e = True


        