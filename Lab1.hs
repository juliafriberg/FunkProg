import Test.QuickCheck

-- Part 1

{-
The number of steps used when calculating power n k is k+1, 
since the function is recursive and starts at k and for each 
recursive step takes one less than the previous k, until k is zero. 
This results in k+1 steps.
-}

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Part 2

power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power1: negative argument"
power1 n 0 =1
power1 n k = product (replicate (fromInteger k) (fromInteger n))


-- Part 3

power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power2: negative argument"
power2 n 0 = 1
power2 n k | even k = power2 (n * n) (k `div` 2)
power2 n k = n * power2 n (k-1)

-- Part 4

--A
{-
Testcases:
Odd k 
Even k
Negative n 
n and k zero
Big n and k
Normal n

Odd and even testcases to test the functionality in power2. 
Negative n to make sure they calculate the power correctly for negative 
numbers as well.
K and N are zero to test the base cases.
Big numbers to see that they work for a variety of numbers.
Also normal, a bit smaller, n to test normal cases.

We will only test with integers since that is the only type the function
is defined for. We will not test with negative k since that is not a valid
number and will cause an error.
-}

--B
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = power1 n k == power n k && 
 power2 n k == power n k && power n k == n^k

--C
testFunctions :: [Bool]
testFunctions = [prop_powers n k | n <- [4, -2, 0, 356], 
 k<-[9, 8, 0, 723]]

--D
{-
prop_powers failed since errors are thrown with negative values. 
This is solved by taking the absolute value for k.
-} 
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = prop_powers n (abs k)





