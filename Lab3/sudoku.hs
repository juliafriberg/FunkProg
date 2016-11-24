import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List

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

solved :: Sudoku
solved =
    Sudoku
      [ [j 1,j 2,j 3,j 4,j 5,j 6,j 7,j 8,j 9]
      , [j 4,j 5,j 6,j 7,j 8,j 9,j 1,j 2,j 3]
      , [j 7,j 8,j 9,j 1,j 2,j 3,j 4,j 5,j 6]
      , [j 2,j 1,j 4,j 3,j 6,j 5,j 8,j 9,j 7]
      , [j 3,j 6,j 5,j 8,j 9,j 7,j 2,j 1,j 4]
      , [j 8,j 9,j 7,j 2,j 1,j 4,j 3,j 6,j 5]
      , [j 5,j 3,j 1,j 6,j 4,j 2,j 9,j 7,j 8]
      , [j 6,j 4,j 2,j 9,j 7,j 8,j 5,j 3,j 1]
      , [j 9,j 7,j 8,j 5,j 3,j 1,j 6,j 4,j 2]
      ]
  where
    n = Nothing
    j = Just

-- Lab3A

--Assignment A

-- A1
-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [[n | x<-[1..9]] | x<-[1..9]]
                 where n = Nothing

-- A2
-- isSudoku sud checks if sud is really a valid representation of a sudoku
isSudoku :: Sudoku -> Bool
isSudoku sudoku = length (rows sudoku) == 9 
                  && and [length x == 9 | x <- rows sudoku]
                  && checkAllElements (\y -> isNothing y || 
                    (fromJust y < 10 && fromJust y > 0 )) sudoku

checkAllElements :: (Maybe Int -> Bool) -> Sudoku -> Bool
checkAllElements f sudoku = and [all f x | x <- rows sudoku]

-- A3
-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved = checkAllElements isJust
                  

-- Assignment B

-- B1 
-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = 
    putStr (unlines [[maybe '.' intToDigit x | x <- y ] | y <- rows sudoku])


-- B2
-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filePath = 
  do 
    rowList <- readFile filePath
    let rows = lines rowList
    let sudoku = Sudoku [[if c == '.' then Nothing else Just (digitToInt c) 
                                  | c <- row] | row <- rows]
    if isSudoku sudoku then return sudoku
      else error "Not a soduko!"

-- Assignment C

-- C1
-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(1,rNothing),(9,rJust)]
  where
    rNothing = elements [Nothing]
    rJust = do 
         n <- choose(1,9)
         return $ Just n

-- C2
-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- C3
-- tests if a sudoku is a sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku   

-- Assignment D

-- D1
-- Tests if a block is ok, i.e. does not contain the same digit twice
isOkayBlock :: Block -> Bool
isOkayBlock block = length nbrList == length (nub nbrList)
  where 
    nbrList = [x | x <- block, isJust x]


-- D2
-- Given a sudoku, all blocks are returned. 
-- This is 9 rows, 9 columns and 9 3*3 squares.
blocks :: Sudoku -> [Block]
blocks sudoku = rowsInSudoku ++ columnsInSudoku ++ squaresInSudoku
    where 
        rowsInSudoku = rows sudoku
        columnsInSudoku = transpose rowsInSudoku
        squaresInSudoku = [concat [take 3 (drop x row) | 
          row <- take 3 (drop y (rows sudoku))] | x <- [0,3,6], y <- [0,3,6]]



prop_sudoku_blocks :: Sudoku -> Bool
prop_sudoku_blocks sudoku = and [length x == 9 | x <- blocksInSudoku]  
      &&  length blocksInSudoku == 9*3
  where blocksInSudoku = blocks sudoku
  
-- D3
isOkay :: Sudoku -> Bool
isOkay sudoku = and [isOkayBlock x | x <- blocks sudoku]

-- Lab3B
-- Assignment E

-- E1
-- Given a sudokureturns a list of the positions in the sudoku that are still blank
blanks :: Sudoku -> [Pos]
blanks sudoku = [(x,y) | x <- [0..8], y <- [0..8], isNothing ((rows sudoku !! x) !! y)]

prop_allBlank :: Sudoku -> Bool
prop_allBlank sudoku = and [isNothing ((rows sudoku !! row) !! col) | (row,col) <- blanks sudoku]

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

