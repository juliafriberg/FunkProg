import Test.QuickCheck

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
type Block = [Maybe Int]
type Pos = (Int,Int)

-- Lab3A

--Assignment A

-- A1
allBlankSudoku :: Sudoku
allBlankSudoku = undefined

-- A2
isSudoku :: Sudoku -> Bool
isSudoku = undefined

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

