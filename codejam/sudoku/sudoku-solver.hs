module Main where

import qualified Data.Set as Set
import Data.List (sort,transpose)
import System.Environment
import Text.ParserCombinators.Parsec

-- This has to be in a utility somewhere...
arraySize a = foldl (+) 0 (map (\x -> 1) a)

{-|

BoardEntry Datastructure

|-}

data BoardEntry = Unknown (Set.Set Int) | Known Int deriving Show

instance Eq BoardEntry where
         Known x == Known y      = x == y
         Unknown x == Unknown y  = x == y
         _ == _                  = False

knownVal :: BoardEntry -> Int
knownVal (Known v) = v
knownVal (Unknown v) = 0

isKnown :: BoardEntry -> Bool
isKnown (Unknown _) = False
isKnown (Known _) = True

isMaybe :: BoardEntry -> Int -> Bool
isMaybe (Known x) y = x == y
isMaybe (Unknown set) x = Set.member x set

{-|

Board Datastructure

|-}

data Board = Board { by_row::[[BoardEntry]] } deriving Show

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
boxRowsByRow rows = (boxRowsByCol $ fst split) ++ (boxRowsByRow $ snd split)
             where split = splitAt 3 rows

boxRowsByCol :: [[BoardEntry]] -> [[[BoardEntry]]]
boxRowsByCol [] = []
boxRowsByCol rows = (map fst split):(boxRowsByCol (filter (\x -> (length x) > 0) (map snd split)))
                    where split = map (splitAt 3) rows

unboxBoard :: Board -> Board
unboxBoard = boxBoard -- own inverse?

isSolved b = foldl (foldl (\x -> \y -> x && (isKnown y))) True (by_row b)


{-|

Board manipulation

|-}

solveBoard :: Board -> (Board, Int)
solveBoard b = solveBoardStep b 1

solveBoardStep :: Board -> Int -> (Board, Int)
solveBoardStep b step = trySolveStep b step

trySolveStep :: Board -> Int -> (Board, Int)
trySolveStep b step = let (collapsedBoard, didWork) = collapseKnown $ restrictBoard b
                      in
                      if didWork
                      then
                          trySolveStep collapsedBoard (step + 1)
                      else
                          let (onliedBoard, didWork) = collapseKnown $ checkOnly collapsedBoard
                          in
                          if didWork
                          then
                              trySolveStep onliedBoard (step + 1)
                          else
                              (onliedBoard, step)


boardFold :: ([BoardEntry] -> [BoardEntry]) -> Board -> Board
boardFold singleRowFn b = let byRowFn = \x -> Board $ map singleRowFn (by_row x)
                              byColFn = uncolumnBoard . byRowFn . columnBoard
                              byBoxFn = unboxBoard . byRowFn . boxBoard
                          in
                          boardFoldList b [ byBoxFn, byColFn, byRowFn ]

boardFoldList :: Board -> [ (Board -> Board) ] -> Board
boardFoldList b fnList = foldl (\x -> \y -> y x) b fnList

didWorkFold :: (Board,Bool) -> (Board -> (Board,Bool)) -> (Board, Bool)
didWorkFold (b,didwork) fn = let (newboard,newdid) = fn b
                                 in
                                 (newboard, newdid || didwork)

restrictBoard :: Board -> Board
restrictBoard = boardFold restrictSingleRow

restrictSingleRow :: [BoardEntry] -> [BoardEntry]
restrictSingleRow r = map (\x -> removeKnown x knownVals) r
                  where knownVals = map knownVal (filter isKnown r)

removeKnown :: BoardEntry -> [Int] -> BoardEntry
removeKnown (Known x) _ = Known x
removeKnown (Unknown set) knownVals = Unknown (foldr Set.delete set knownVals)

couldCollapse :: Board -> Bool
couldCollapse b = any (any couldCollapseSingle) (by_row b)

couldCollapseSingle :: BoardEntry -> Bool
couldCollapseSingle (Known _) = False
couldCollapseSingle (Unknown set) = Set.size set == 1

collapseKnown :: Board -> (Board, Bool)
collapseKnown b = (Board (map (map fst) r), any (any snd) r)
                where r = map (map collapseSingleKnown) (by_row b)

collapseSingleKnown :: BoardEntry -> (BoardEntry, Bool)
collapseSingleKnown (Known v) = (Known v, False)
collapseSingleKnown (Unknown set) = if Set.size set == 1
                                    then
                                        (Known ((Set.toList set)!!0),True)
                                    else
                                        (Unknown set, False)

checkOnly :: Board -> Board
checkOnly = boardFold checkOnlySingleRow

checkOnlySingleRow :: [BoardEntry] -> [BoardEntry]
checkOnlySingleRow row = let singles = filter (\maybe -> arraySize( filter (\set -> isMaybe set maybe) row ) == 1) [1..9]
                         in
                         map (\x -> knownFromOnly x singles) row

knownFromOnly :: BoardEntry -> [Int] -> BoardEntry
knownFromOnly (Known x) _ = Known x
knownFromOnly (Unknown set) [] = Unknown set
knownFromOnly (Unknown set) singles = let fromSingles = filter (\x -> Set.member x set) singles
                                      in
                                      if arraySize( fromSingles ) == 1
                                      then
                                          Unknown (Set.fromList fromSingles) -- fromSingles should be a single entry
                                      else
                                          Unknown set

checkPairs :: Board -> Board
checkPairs = boardFold checkPairsSingleRow

isPair :: BoardEntry -> Bool
isPair (Known _) = False
isPair (Unknown set) = Set.size set == 2

checkPairsSingleRow :: [BoardEntry] -> [BoardEntry]
checkPairsSingleRow row = let pairs = filter isPair row
                          in
                          if arraySize pairs == 2 && (pairs!!0) == (pairs!!1)
                          then
                              map (\x -> if x == (pairs!!0) then x else checkPairsRemove x (pairs!!0)) row
                          else
                              row

checkPairsRemove :: BoardEntry -> BoardEntry -> BoardEntry
checkPairsRemove (Unknown from) (Unknown toRemove) = Unknown $ foldr Set.delete from (Set.elems toRemove)
checkPairsRemove from _ = from

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

showBoard b = let formatWidths = colWidths b
              in
              (showBreak formatWidths) ++ 
              (showManyRows (by_row b) formatWidths)

showBreak widths = "+" ++ (join "+" $ showBreakBox widths) ++ "+\n"

showBreakBox :: [Int] -> [[Char]]
showBreakBox [] = []
showBreakBox widths = let split = splitAt 3 widths
                      in
                      [ "-" ++ (join "-" $ map (\x -> replicate x '-') (fst split)) ++ "-" ] ++ (showBreakBox (snd split))

showManyRows :: [[BoardEntry]] -> [Int] -> [Char]
showManyRows [] _ = []
showManyRows rs widths = (join "" (map (\x -> showRow x widths) (fst split))) ++
                       (showBreak widths) ++
                       (showManyRows (snd split) widths)
                       where split = splitAt 3 rs

showRow :: [BoardEntry] -> [Int] -> [Char]
showRow r widths = "|" ++ (showRowPieces r widths) ++ "\n"

showRowPieces :: [BoardEntry] -> [Int] -> [Char]
showRowPieces [] _ = []
showRowPieces r widths = (showRowBox (fst rowSplit) (fst widthSplit)) ++ "|" ++ (showRowPieces (snd rowSplit) (snd widthSplit))
                       where rowSplit = splitAt 3 r
                             widthSplit = splitAt 3 widths

showRowBox :: [BoardEntry] -> [Int] -> [Char]
showRowBox rb widths = " " ++ (join " " (map showColFmt (zip rb widths))) ++ " "

showColFmt :: (BoardEntry,Int) -> [Char]
showColFmt (e,w) = let shown = showCol e
                   in
                   shown ++ (replicate (w - (arraySize shown)) ' ')

showCol :: BoardEntry -> [Char]
showCol (Known i) = show i
showCol (Unknown c) = (showColList . Set.toAscList) c

showColList [] = "[X]"
showColList [1,2,3,4,5,6,7,8,9] = "[*]"
showColList a = if arraySize a > 4
                then
                   "~" ++ (showColList (Set.toAscList (Set.difference (Set.fromList [1..9]) (Set.fromList a))))
                else
                   show a

colWidths :: Board -> [Int]
colWidths b = map (maximum . map widthOfCol) (by_row $ columnBoard b)

widthOfCol :: BoardEntry -> Int
widthOfCol b = arraySize (showCol b)

{-|

Main

|-}

main = do
     args <- getArgs
     input <- getContents
     let parsedBoard = case parse parseInput "stdin"  input of
                       Left  err -> error $ "Input:\n" ++ show input ++ 
                                            "\nError:\n" ++ show err
                       Right result -> result
     let workBoard = buildWork parsedBoard
     let (maybeSolvedBoard, steps) = solveBoard workBoard
     let shouldDebug = any (\x -> x == "-d" || x == "--debug") args

     if shouldDebug
     then do
        putStrLn $ "Initial Board\n" ++ (showBoard workBoard)
        putStr $ "Supposedly Solved Board\n" ++
               "  steps=" ++ (show steps) ++ "\n" ++ 
               "  solved=" ++ (show $ isSolved maybeSolvedBoard) ++ "\n" ++
               (showBoard maybeSolvedBoard)
     else
        putStr $ showBoard maybeSolvedBoard

     putStr $ "CheckPairs Board\n"
     putStr $ showBoard (checkPairs maybeSolvedBoard)
