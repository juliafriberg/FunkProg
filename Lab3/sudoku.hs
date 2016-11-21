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
isSudoku sudoku = (length $ rows sudoku) == 9 
                  && (and (map (\x -> length x == 9) $ rows sudoku))
                  && (and (map (\x -> 
                     (and (map (\y -> (isNothing y || (fromJust y < 10 && fromJust y > 0))) x))) 
                     $ rows sudoku))  

-- A3
-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sudoku = (and (map (\x -> 
                  (and (map (\y -> not (isNothing y)) x))) 
                  $ rows sudoku))

-- Assignment B

-- B1 
-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = 
  do 
    putStr (unlines [[if isNothing x then '.' else intToDigit (fromJust x) | x <- y ] | y <- (rows sudoku)])


-- B2
-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filePath = 
  do 
    rowList <- readFile filePath
    let rows = lines rowList
    let sudoku = (Sudoku [[if c == '.' then Nothing else Just (digitToInt c)| c <- row] | row <- rows])
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
prop_Sudoku sudoku = isSudoku sudoku  

-- Assignment D

-- D1
-- Tests if a block is ok, i.e. does not contain the same digit twice
isOkayBlock :: Block -> Bool
isOkayBlock block = length nbrList == length (nub nbrList)
  where 
    nbrList = [x | x <- block, not (isNothing x)]


-- D2
-- Given a sudoku, all blocks are returned. This is 9 rows, 9 columns and 9 3*3 squares.
blocks :: Sudoku -> [Block]
blocks sudoku = rowsInSudoku ++ columnsInSudoku ++ getSquares (rows sudoku)
    where 
        rowsInSudoku = [x | x <- (rows sudoku)]
        columnsInSudoku =  [x | x <- transpose (rows sudoku)]
        squaresInSudoku = getSquares (rows sudoku)
        getSquares [] = []
        getSquares xs = getSquaresForRows (transpose (take 3 xs)) ++ getSquares (drop 3 xs)
        getSquaresForRows [] = []
        getSquaresForRows xs = [concat (take 3 xs)] ++ getSquaresForRows (drop 3 xs)

prop_sudoku_blocks :: Sudoku -> Bool
prop_sudoku_blocks = undefined
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

