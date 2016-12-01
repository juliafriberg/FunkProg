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
-- if the file did not contain a qsudoku
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
cell = frequency [(9,return Nothing),(1,rJust)]
  where
    rJust = do 
         n <- choose(1,9)
         return $ Just n

-- C2
-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9]]
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
        squaresInSudoku = [concat [take 3 (drop (3*x) row) 
                  | row <- take 3 (drop (3*y) (rows sudoku))] 
                  | y <- [0,1,2], x <- [0,1,2]]


-- Tests function blocks by checking that there are 27 blocks 
-- and that each block consists of 9 elements 
prop_sudoku_blocks :: Sudoku -> Bool
prop_sudoku_blocks sudoku = and [length x == 9 | x <- blocksInSudoku]  
      &&  length blocksInSudoku == 9*3
  where blocksInSudoku = blocks sudoku
  
-- D3
-- Given a sudoku, checks that the sudoku is a proper sudoku
isOkay :: Sudoku -> Bool
isOkay sudoku = and [isOkayBlock x | x <- blocks sudoku]

-- Lab3B
-- Assignment E

-- E1
-- Given a sudoku, returns a list of the positions in the sudoku 
-- that are still blank
blanks :: Sudoku -> [Pos]
blanks sudoku = [(x,y) | x <- [0..8], y <- [0..8], 
                        isNothing ((rows sudoku !! x) !! y)]

-- Tests the function blanks
prop_allBlank :: Sudoku -> Bool
prop_allBlank sudoku = and [isNothing ((rows sudoku !! row) !! col) 
                              | (row,col) <- blanks sudoku]

-- E2
--  Updates the given list with the new value at the given index.
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (index, newX) 
    | index >= length xs || index < 0 = error "Index outside of list"
    | otherwise = [if x == index 
      then newX 
        else xs !! x | x <- [0..(length xs-1)]]

-- Tests the function !!= by checking the length of the given list and the 
-- length of the list after the swap as well as checks that the other values
-- in the list are the same after the swap.
prop_swap :: [Integer] -> (Int, Integer) -> Bool
prop_swap xs (index, newX) = (length xs == length newList) && 
    take (index-1) xs == take (index-1) newList && 
            drop (index+1) xs == drop (index+1) newList
  where 
    newList = xs !!= (index, newX)


-- E3
-- Updates the given Sudoku at the given position with the new value.
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sudoku (row,col) val = 
    Sudoku (rows sudoku !!= (row, (rows sudoku !! row) !!= (col, val)))


-- Tests the function update
prop_update :: Sudoku -> Maybe Int -> Property
prop_update sudoku val = forAll rPos (\pos -> prop_update' sudoku pos val)
  where 
    prop_update' sudoku (row,col) val = 
          (rows (update sudoku (row,col) val) !! row) !! col == val

-- Generates a valid position
rPos :: Gen Pos
rPos = 
    do 
      row <- choose(0,8)
      col <- choose(0,8)
      return (row, col)

-- E4
-- Determines which numbers could be legally written into given position.
candidates :: Sudoku -> Pos -> [Int]
candidates sudoku (row, col) =  notInRow `intersect` 
                                notInCol `intersect` 
                                notInSquare
    where notInList xs = [fromJust x | x <- ([Just x 
                                | x <- [1..9]] ++ [Nothing]) \\ xs]
          notInRow = notInList (rows sudoku !! row)
          notInCol = notInList (transpose (rows sudoku) !! col)
          notInSquare = notInList (getSquare row col)
          getSquare x y = concat [take 3 (drop (getInterval y) row) 
            | row <- take 3 (drop (getInterval x) (rows sudoku))]
          getInterval x 
                  | x < 3 = 0
                  | x < 6 = 3
                  | x < 9 = 6

-- Tests the function candidates by checking that the sudoku still is 
-- valid after updating with a candidate
prop_candidates :: Sudoku -> Property 
prop_candidates sudoku = isOkay sudoku ==> 
    forAll rPos (prop_candidates' sudoku)
    where
      prop_candidates' sudoku pos = isOkay sud && isSudoku sud 
        where sud = update sudoku pos (Just (head $ candidates sudoku pos))


-- Assignment F

-- F1
-- Solves a sudoku
solve :: Sudoku -> Maybe Sudoku
solve sudoku 
    | isSudoku sudoku && isOkay sudoku = solve' sudoku (blanks sudoku)
    | otherwise = Nothing
    where
      solve' sud [] = Just sud
      solve' sud (blank:blanks) = 
        listToMaybe $ catMaybes 
        [solve' (update sud blank (Just candidate)) blanks 
          | candidate <- candidates sud blank]

-- F2
-- Reads a sudoku from given file, solves it and print the answer.
readAndSolve :: FilePath -> IO ()
readAndSolve file = 
  do 
    sud <- readSudoku file  
    let solved = solve sud
    maybe (putStrLn "No solution") printSudoku solved



-- F3
-- Checks that the first sudoku is a solution of the second sudoku 
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sud1 sud2 = isOkay sud1 && isSolved sud1 && 
    and [x == ((rows sud1 !! row) !! col) 
      | col <- [0..8], row <- [0..8], 
        let x = (rows sud2 !! row) !! col, isJust x]

-- F4
-- Tests function solve by checking that the solution from solve is a 
-- valid solution using isSolutionOf
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sudoku = isSudoku sudoku && isJust sud ==> 
  isSolutionOf (fromJust sud) sudoku
  where 
    sud = solve sudoku 


