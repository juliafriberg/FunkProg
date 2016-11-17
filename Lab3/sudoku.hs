import Test.QuickCheck
import Data.Maybe

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
        deriving (Eq, Show)
type Block = [Maybe Int]
type Pos = (Int,Int)

example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- Lab3A

--Assignment A

-- A1
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [[n | x<-[1..9]] | x<-[1..9]]
                 where n = Nothing

-- A2
isSudoku :: Sudoku -> Bool
isSudoku sudoku = (length $ rows sudoku) == 9 
                  && (and (map (\x -> length x == 9) $ rows sudoku))
                  && (and (map (\x -> 
                     (and (map (\y -> (isNothing y || (fromJust y < 10 && fromJust y > 0))) x))) 
                     $ rows sudoku))  

-- A3
isSolved :: Sudoku -> Bool
isSolved = undefined

-- Assignment B

-- B1 
printSudoku :: Sudoku -> IO ()
printSudoku = undefined

-- B2
readSudoku :: FilePath -> IO Sudoku
readSudoku = undefined

-- Assignment C

-- C1
cell :: Gen (Maybe Int)
cell = undefined

-- C2. Make Sudokus an instance of the class Arbitrary.
--instance Arbitrary Sudoku where

-- C3
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = undefined

-- Assignment D

-- D1
isOkayBlock :: Block -> Bool
isOkayBlock = undefined

-- D2
blocks :: Sudoku -> [Block]
blocks = undefined


-- D3
isOkay :: Sudoku -> Bool
isOkay = undefined

-- Lab3B

-- Assignment E

-- E1
blanks :: Sudoku -> [Pos]
blanks = undefined

-- E2
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) = undefined

-- E3
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update = undefined


-- E4
candidates :: Sudoku -> Pos -> [Int]
candidates = undefined

-- Assignment F

-- F1
solve :: Sudoku -> Maybe Sudoku
solve = undefined

-- F2
readAndSolve :: FilePath -> IO ()
readAndSolve = undefined

-- F3
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf = undefined

-- F4
prop_SolveSound :: Sudoku -> Property
prop_SolveSound = undefined

