module Main where

import qualified Data.Set as Set
import Data.List (sort,sortBy,transpose)
import System.Environment
import Text.ParserCombinators.Parsec

{-|

BoardEntry Datastructure

|-}

data BoardEntry = Unknown (Set.Set Int) | Known Int deriving Show

instance Eq BoardEntry where
         Known x == Known y      = x == y
         Unknown x == Unknown y  = x == y
         _ == _                  = False

instance Ord BoardEntry where
         Known x `compare` Known y = x `compare` y
         Unknown x `compare` Unknown y = Set.size x `compare` Set.size y
         Known x `compare` Unknown y = GT
         Unknown x `compare` Known y = LT

knownVal :: BoardEntry -> Maybe Int
knownVal (Unknown v) = Nothing
knownVal (Known v) = Just v

isKnown :: BoardEntry -> Bool
isKnown (Unknown _) = False
isKnown (Known _) = True

isMaybe :: BoardEntry -> Int -> Bool
isMaybe (Known x) y = x == y
isMaybe (Unknown set) x = Set.member x set

unknownSet :: BoardEntry -> Maybe (Set.Set Int)
unknownSet (Known _) = Nothing
unknownSet (Unknown set) = Just set

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

boardPosPairs :: Board -> [ (BoardEntry, (Int,Int)) ]
boardPosPairs b = concat $ map boardPosPairsCols (boardPosPairsRows b)

boardPosPairsRows :: Board -> [ ([BoardEntry], Int) ]
boardPosPairsRows b = zip (by_row b) [0..8]

boardPosPairsCols :: ( [BoardEntry], Int ) -> [ (BoardEntry,(Int,Int)) ]
boardPosPairsCols ( row, row_pos ) = map (\x->let col=(fst x); col_pos=(snd x) in (col,(row_pos,col_pos))) $ zip row [0..8]

{-|

|-}

data SolveReturn = SolveReturn { workBoard::Board, workSteps::Int, workGuesses::Int } deriving Show


{-|

Board manipulation

|-}

solveBoard :: Board -> SolveReturn
solveBoard b = solveBoardStep (SolveReturn b 0 0)

solveBoardStep :: SolveReturn -> SolveReturn
solveBoardStep work = let (maybeSolved, totalSteps) = trySolveStep (workBoard work) (workSteps work)
                          newWork = SolveReturn maybeSolved totalSteps (workGuesses work)
                      in
                      if isSolved maybeSolved
                      then
                          newWork
                      else
                          tryGuesses newWork


trySolveStep :: Board -> Int -> (Board, Int)
trySolveStep b step = let (collapsedBoard, didWork) = collapseKnown $ restrictBoard b
                      in
                      if didWork
                      then
                          trySolveStep collapsedBoard (step + 1)
                      else
                          let (onliedBoard, didWork2) = collapseKnown $ checkOnly collapsedBoard
                          in
                          if didWork2
                          then
                              trySolveStep onliedBoard (step + 1)
                          else
                              (onliedBoard, step + 1)


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

{-|

guess -- find the smallest (Unknown set) on the board and try all values. then recursively solve

|-}

tryGuesses :: SolveReturn -> SolveReturn
tryGuesses work = tryWithGuesses work (makeGuesses (workBoard work))

data Guesses = Guesses { guessSet::(Set.Set Int), guessPos::(Int,Int) } deriving Show

boardEntryToGuess :: (BoardEntry, (Int,Int)) -> Maybe Guesses
boardEntryToGuess ((Known _),_) = Nothing
boardEntryToGuess ((Unknown set),pos) = Just $ Guesses set pos

makeGuesses :: Board -> Maybe Guesses
makeGuesses b = boardEntryToGuess (head $ sortBy guessOrder (boardPosPairs b))
--makeGuesses b = Nothing

chainOrdering :: Ordering -> Ordering -> Ordering
chainOrdering EQ order = order
chainOrdering order _ = order

guessOrder :: (BoardEntry,(Int,Int)) -> (BoardEntry,(Int,Int)) -> Ordering
guessOrder ((Known _),_) ((Unknown _),_) = GT
guessOrder ((Unknown _),_) ((Known _),_) = LT
guessOrder ((Known v1),(x1,y1)) ((Known v2,(x2,y2))) = foldl chainOrdering EQ [v1 `compare` v2, x1 `compare` x2, y1 `compare` y2]
guessOrder ((Unknown s1),(x1,y1)) ((Unknown s2,(x2,y2))) = foldl chainOrdering EQ [(Set.size s1) `compare` (Set.size s2), x1 `compare` x2, y1 `compare` y2]

solveReturnFold :: SolveReturn -> SolveReturn -> SolveReturn
solveReturnFold a b = a{workGuesses = (workGuesses a) + (workGuesses b), workSteps = (workSteps a) + (workSteps b)}

tryWithGuesses :: SolveReturn -> Maybe Guesses -> SolveReturn
tryWithGuesses work Nothing = work
tryWithGuesses work (Just g) = let guessBoards = makeGuessBoards (workBoard work) g
                                   attempts = map solveBoard guessBoards
                                   solved = filter (\x -> isSolved (workBoard x)) attempts
                                   return = foldl solveReturnFold work{workGuesses = (workGuesses work) + 1} attempts
                               in
                               if length solved == 1
                               then
                                   let found = head solved
                                   in
                                   return{workBoard = (workBoard found)}
                               else
                                   return
  
makeGuessBoards :: Board -> Guesses -> [Board]
makeGuessBoards b guesses = map (applyGuess b (guessPos guesses)) (Set.toList $ guessSet guesses)

applyGuess :: Board -> (Int,Int) -> Int -> Board
applyGuess b (x_pos,y_pos) v = let rowSplit = splitAt x_pos (by_row b)
                                   colSplit = splitAt y_pos (head $ snd rowSplit)
                               in
                               Board $ (fst rowSplit) ++ [(fst colSplit) ++ [ Known v ] ++ (tail $ snd colSplit)] ++ (tail $snd rowSplit)

{-|

restrictBoard -- take (Known x) out if (Unknown set) by row/col/box

|-}

restrictBoard :: Board -> Board
restrictBoard = boardFold restrictSingleRow

restrictSingleRow :: [BoardEntry] -> [BoardEntry]
restrictSingleRow r = map (\x -> removeKnown x knownVals) r
                  where knownVals = map knownVal (filter isKnown r)

removeKnown :: BoardEntry -> [Maybe Int] -> BoardEntry
removeKnown (Known x) _ = Known x
removeKnown (Unknown set) knownVals = Unknown (foldr removeKnownSingle set knownVals)

removeKnownSingle :: Maybe Int -> Set.Set Int -> Set.Set Int
removeKnownSingle Nothing s = s
removeKnownSingle (Just v) s = Set.delete v s

{-|

collapseKnown -- turn (Unknown (Set.singleton x)) into (Known x)

|-}

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
                                        (Known (head (Set.toList set)),True)
                                    else
                                        (Unknown set, False)

{-|

checkOnly -- find instances where there is only one (Unknown x) by box/col/row that contains a given value

|-}

checkOnly :: Board -> Board
checkOnly = boardFold checkOnlySingleRow

checkOnlySingleRow :: [BoardEntry] -> [BoardEntry]
checkOnlySingleRow row = let singles = filter (\maybe -> length ( filter (\set -> isMaybe set maybe) row ) == 1) [1..9]
                         in
                         map (\x -> knownFromOnly x singles) row

knownFromOnly :: BoardEntry -> [Int] -> BoardEntry
knownFromOnly (Known x) _ = Known x
knownFromOnly (Unknown set) [] = Unknown set
knownFromOnly (Unknown set) singles = let fromSingles = filter (\x -> Set.member x set) singles
                                      in
                                      if length fromSingles == 1
                                      then
                                          Unknown (Set.fromList fromSingles) -- fromSingles should be a single entry
                                      else
                                          Unknown set

{-|

checkPairs -- find instances where [a,b] is in a box/col/row twice and remove a and b from the other (Unknown set)s

|-}

checkPairs :: Board -> Board
checkPairs = boardFold checkPairsSingleRow

isPair :: BoardEntry -> Bool
isPair (Known _) = False
isPair (Unknown set) = Set.size set == 2

checkPairsSingleRow :: [BoardEntry] -> [BoardEntry]
checkPairsSingleRow row = let pairs = filter isPair row
                          in
                          if length pairs == 2 && (pairs!!0) == (pairs!!1)
                          then
                              map (\x -> if x == (head pairs) then x else checkPairsRemove x (head pairs)) row
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
                   shown ++ (replicate (w - (length shown)) ' ')

showCol :: BoardEntry -> [Char]
showCol (Known i) = show i
showCol (Unknown c) = (showColList . Set.toAscList) c

showColList [] = "[X]"
showColList [1,2,3,4,5,6,7,8,9] = "[*]"
showColList a = if length a > 4
                then
                   "~" ++ (showColList (Set.toAscList (Set.difference (Set.fromList [1..9]) (Set.fromList a))))
                else
                   show a

colWidths :: Board -> [Int]
colWidths b = map (maximum . map widthOfCol) (by_row $ columnBoard b)

widthOfCol :: BoardEntry -> Int
widthOfCol b = length $ showCol b

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
     let maybeSolved = solveBoard (buildWork parsedBoard)
     let shouldDebug = any (\x -> x == "-d" || x == "--debug") args

     if shouldDebug
     then do
        putStrLn $ "Initial Board\n" ++ (showBoard $ buildWork parsedBoard)
        putStr $ "Supposedly Solved Board\n" ++
               "  steps=" ++ (show (workSteps maybeSolved)) ++ "\n" ++ 
               "  guesses=" ++ (show (workGuesses maybeSolved)) ++ "\n" ++ 
               "  solved=" ++ (show $ isSolved (workBoard maybeSolved)) ++ "\n" ++
               (showBoard $ workBoard maybeSolved)
     else
        putStr $ showBoard (workBoard maybeSolved)
