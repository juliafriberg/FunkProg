module ExprQC where

import Test.QuickCheck
import Expr
import Data.List

arbExpr :: Int -> Gen Expr
arbExpr n 
    | n == 0 = rNum
    | otherwise = oneof [rAdd, rMul, return Var, rSin, rCos, rNum]
    where
        rNum = do
            a <- elements [1..10 :: Double]
            b <- elements [1..10 :: Double] 
            return $ Lit (a) 
        rAdd = do
            term1 <- arbExpr size
            term2 <- arbExpr size
            return (Add term1 term2)
        rMul = do
            factor1 <- arbExpr size
            factor2 <- arbExpr size
            return (Mul factor1 factor2) 
        rSin = do
            expr <- arbExpr (n-1)
            return (Sin expr)
        rCos = do
            expr <- arbExpr (n-1)
            return (Cos expr)
        size = n `div` 2

instance Arbitrary Expr where
  arbitrary = sized arbExpr