module Main where

import qualified Data.Set as Set
import Data.List (sort,transpose)
import Text.ParserCombinators.Parsec

{-|

Board Datastructure

|-}

data BoardEntry = Unknown (Set.Set Int) | Known Int deriving Show
data Board = Board { by_row::[[BoardEntry]] } deriving Show

knownVal :: BoardEntry -> Int
knownVal (Known v) = v
knownVal (Unknown v) = 0

isKnown :: BoardEntry -> Bool
isKnown (Unknown _) = False
isKnown (Known _) = True

buildWork :: [[Int]] -> Board
buildWork b = Board (map (map (\x -> if x == 0 then Unknown (Set.fromList [1..9]) else Known x) ) b)

columnBoard :: Board -> Board
columnBoard b = (Board . transpose . by_row) b

uncolumnBoard :: Board -> Board
uncolumnBoard = columnBoard -- transpose is it's own inverse

boxBoard :: Board -> Board
boxBoard b = (Board . boxRows . by_row) b

boxRows :: [[BoardEntry]] -> [[BoardEntry]]
boxRows rows = map concat $ boxRowsByRow rows

boxRowsByRow :: [[BoardEntry]] -> [[[BoardEntry]]]
boxRowsByRow [] = []
boxRowsByRow rows = (boxRowsByCol (fst split)) ++ (boxRowsByRow (snd split))
             where split = splitAt 3 rows

boxRowsByCol :: [[BoardEntry]] -> [[[BoardEntry]]]
boxRowsByCol [] = []
boxRowsByCol rows = (map fst split):(boxRowsByCol (filter (\x -> (length x) > 0) (map snd split)))
                    where split = map (splitAt 3) rows

unboxBoard :: Board -> Board
unboxBoard = boxBoard -- own inverse?

{-|

Board manipulation

|-}

solveBoard :: Board -> Board
solveBoard b = if didWork
               then
                  solveBoard collapsedBoard
               else
                  collapsedBoard
               where (collapsedBoard, didWork) = collapseKnown (restrictBoard b)

restrictBoard :: Board -> Board
restrictBoard b = (restrictCols . restrictRows . restrictBoxes) b

restrictRows :: Board -> Board
restrictRows b = Board $ map restrictSingleRow (by_row b)

restrictSingleRow :: [BoardEntry] -> [BoardEntry]
restrictSingleRow r = map (\x -> removeKnown x knownVals) r
                  where knownVals = map knownVal (filter isKnown r)

removeKnown :: BoardEntry -> [Int] -> BoardEntry
removeKnown (Known x) _ = Known x
removeKnown (Unknown set) knownVals = Unknown (foldr Set.delete set knownVals)

restrictCols :: Board -> Board
restrictCols b = (uncolumnBoard . restrictRows . columnBoard) b

restrictBoxes :: Board -> Board
restrictBoxes b = (unboxBoard . restrictRows . boxBoard) b

couldCollapse :: Board -> Bool
couldCollapse b = any (any couldCollapseSingle) (by_row b)

couldCollapseSingle :: BoardEntry -> Bool
couldCollapseSingle (Known _) = False
couldCollapseSingle (Unknown set) = Set.size set == 1

collapseKnown :: Board -> (Board, Bool)
collapseKnown b = (Board (map (map fst) r), foldl (foldl (\x -> \y -> x || (snd y))) False r)
                where r = (map (map collapseSingleKnown) (by_row b))

collapseSingleKnown :: BoardEntry -> (BoardEntry, Bool)
collapseSingleKnown (Known v) = (Known v, False)
collapseSingleKnown (Unknown set) = if Set.size set == 1
                                    then
                                        (Known ((Set.toList set)!!0),True)
                                    else
                                        (Unknown set, False)

{-|

Parser Functions

|-}

parseInput = parseBoard

parseBox = (char '_' >> return 0)
         <|> fmap read (many1 digit)

parseBoard = do
           rows <- count (3 * 3) (parseRow (3 * 3))
           return rows

parseRow size = do
         col1 <- parseBox
         colRest <- count (size - 1) parseContinueCol
         newline
         return (col1:colRest)

parseContinueCol = do
                 spaces
                 col <- parseBox
                 return col

{-|

Shower

|-}

join s a = foldl (\x -> \y -> if x == "" then y else x ++ s ++ y) "" a

showBoard b = showManyRows (by_row b)

showManyRows :: [[BoardEntry]] -> [Char]
showManyRows [] = []
showManyRows rs = (join "\n" (map showRow (fst split))) ++
                  "\n---\n" ++
                  (showManyRows (snd split))
             where split = splitAt 3 rs

showRow :: [BoardEntry] -> [Char]
showRow [] = []
showRow r = (showRowBox (fst split)) ++ "  |  " ++ (showRow (snd split))
        where split = splitAt 3 r

showRowBox :: [BoardEntry] -> [Char]
showRowBox rb = join ", " (map showCol rb)

showCol :: BoardEntry -> [Char]
showCol (Known i) = show i
showCol (Unknown c) = (showColList . Set.toAscList) c

arraySize a = foldl (+) 0 (map (\x -> 1) a)

showColList [] = "[X]"
showColList [1,2,3,4,5,6,7,8,9] = "[*]"
showColList a = if arraySize a > 4
                then
                   "~" ++ (showColList (Set.toAscList (Set.difference (Set.fromList [1..9]) (Set.fromList a))))
                else
                   show a



{-|

Main

|-}

main = do
     input <- getContents
     let parsedBoard = case parse parseInput "stdin"  input of
                       Left  err -> error $ "Input:\n" ++ show input ++ 
                                            "\nError:\n" ++ show err
                       Right result -> result
     let workBoard = buildWork parsedBoard

     putStrLn $ "Initial Board:\n" ++ showBoard workBoard
     putStrLn $ "Solved? Board:\n" ++ showBoard (solveBoard workBoard)
     --putStrLn $ "First Restrict:\n" ++ showBoard (restrictBoard workBoard)
     --putStrLn $ "First Collapse:\n" ++ showBoard (collapseKnown (restrictBoard workBoard))
     --putStrLn $ "Second Restrict:\n" ++ showBoard (restrictBoard (collapseKnown (restrictBoard workBoard)))
