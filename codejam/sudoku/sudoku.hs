module Main where

import Data.Ix;
import Data.List (sort,transpose)
import Text.ParserCombinators.Parsec

{-|

Board Datastructure

|-}

data Board = Board { size::Int, by_row::[ [Int] ] } deriving Show

row_count :: Board -> Int
row_count board = (size board) * (size board)

allRows :: Board -> [[Int]]
allRows board = (by_row board)

col_count :: Board -> Int
col_count board = (size board) * (size board)

allColumns :: Board -> [[Int]]
allColumns board = colsFromRows (by_row board)

colsFromRows :: [[Int]] -> [[Int]]
colsFromRows rows = transpose rows

boxx_count :: Board -> Int
boxx_count board = (size board)

boxy_count :: Board -> Int
boxy_count board = (size board)

allBoxes :: Board -> [[Int]]
allBoxes board = map concat (allBoxesRows (size board) (by_row board))

-- allBoxesRows( 2, [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ) = [[[1,2],[1,2]],[[3,4],[3,4]],[[1,2],[1,2]],[[3,4],[3,4]]]
allBoxesRows :: Int -> [[Int]] -> [[[Int]]]
allBoxesRows _ [] = []
allBoxesRows s rows = (allBoxesCols s (fst split)) ++ (allBoxesRows s (snd split)) 
                    where split = splitAt s rows

-- allBoxesCols( 2, [[1,2,3,4],[1,2,3,4]] ) = [[[1,2],[1,2]],[[3,4],[3,4]]]
allBoxesCols :: Int -> [[Int]] -> [[[Int]]]
allBoxesCols _ [] = []
allBoxesCols s rows = (map fst split):(allBoxesCols s (filter (\x -> (length x) > 0) (map snd split)))
                    where split = map (splitAt s) rows


{-|

Parser Functions

|-}

natural = fmap read (many1 digit)

parseInput = do
           tests <- natural
           newline
           boards <- count tests parseBoard
           eof :: Parser()
           return boards

parseBoard = do
           size <- natural
           newline
           rows <- count (size * size) (parseRow (size * size))
           return (Board size rows)

parseRow size = do
         col1 <- natural
         colRest <- count (size - 1) parseContinueCol
         newline
         return (col1:colRest)

parseContinueCol = do
                 spaces
                 col <- natural
                 return col

{-|

Test Functions

|-}

testBoardList boards = map isValidBoard boards

isValidBoard board = (allColumnsValid board) && (allRowsValid board) && (allBoxesValid board)

allColumnsValid :: Board -> Bool
allColumnsValid board = foldl (&&) True (eachColumnValid board)

eachColumnValid :: Board -> [Bool]
eachColumnValid board = map isValidColumn (allColumns board)

allRowsValid :: Board -> Bool
allRowsValid board = foldl (&&) True (eachRowValid board)

eachRowValid :: Board -> [Bool]
eachRowValid board = map isValidRow (allRows board)

allBoxesValid :: Board -> Bool
allBoxesValid board = foldl (&&) True (eachBoxValid board)

eachBoxValid :: Board -> [Bool]
eachBoxValid board = map isValidBox (allBoxes board)

isValidChunk :: [Int] -> Bool
isValidChunk chunk = isValidChunkSorted 1 sortedChunk 
                   where sortedChunk = sort chunk

isValidChunkSorted :: Int -> [Int] -> Bool
isValidChunkSorted x [] = True
isValidChunkSorted x (n:rest) = (x == n) && (isValidChunkSorted (x+1) rest)

isValidColumn col = isValidChunk col
isValidRow row = isValidChunk row
isValidBox box = isValidChunk box

{-|

Test Result Printer

|-}


showResults tests = showResultListN 1 tests

showResultListN n [] = putStr ""
showResultListN n (result:rest) = do
                                showResult n result
                                showResultListN (n+1) rest

showResult n result = putStrLn $ "Case #" ++ (show n) ++ ": " ++ (if result then "Yes" else "No")

{-|

Main

|-}

main = do
     input <- getContents
     let boards = case parse parseInput "stdin"  input of
                  Left  err -> error $ "Input:\n" ++ show input ++ 
                                       "\nError:\n" ++ show err
                  Right result -> result
     {-|
     putStrLn $ "Rows: " ++ show (map allRows boards)
     putStrLn $ "Cols: " ++ show (map allColumns boards)
     putStrLn $ "Boxes: " ++ show (map allBoxes boards)
     |-}
     showResults $ testBoardList boards
