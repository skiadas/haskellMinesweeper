module Board (
   Board,
   Cell,
   board,
   get,
   set,
   cellToChar,
   rowToString,
   boardToStrings,
   boardIsDone,
   createHorizontalWall,
   addVerticalWalls,
   addWalls,
   stringAct,
   makeAction,
   buildBoard,
   printBoard
) where

import Types
import Data.Char
import System.IO
import System.Random

type Board = [[Cell]]
type Cell = (Ground, State)

type Height = Int
type Width = Int
type Row = Int
type Column = Int

board :: Height -> Width -> (Row -> Column -> Ground) -> Board
board 0 _ _ = []
board h w f = board (h - 1) w f ++ [row w (f (h - 1))]

row :: Width -> (Column -> Ground) -> [Cell]
row 0 _ = []
row w f = row (w - 1) f ++ [(f (w - 1), Blank)]

get :: Row -> Column -> Board -> Cell
get r c b = (b !! r) !! c

--Selects the correct row [Cell] containing the cell then passes it to setHelper
set :: Row -> Column -> State -> Board -> Board
set row col state (x:xs) | row < 0 || col < 0 = []
                         | row == 0           = (setHelper col state x) : xs
                         | otherwise          = x : (set (row - 1) col state xs)

--Handles modifying the state of the correct Cell in the given row **Should only be called by the set function**
setHelper :: Column -> State -> [Cell] -> [Cell]
setHelper col state (x:xs) | col == 0  = ((fst x), state) : xs
                           | otherwise = x : (setHelper (col - 1) state xs)

--Takes a cell and converts it to a representitive character in Unicode
cellToChar :: Cell -> Char
cellToChar (Mine, Revealed x) = '\128163'
cellToChar (Safe, Revealed x) = intToDigit x
cellToChar (_,Blank) = ' '
cellToChar (_,Flagged) = '\128681'
cellToChar (_,Questioned) = '\63'

rowToString :: [Cell] -> String
rowToString = map cellToChar

boardToStrings :: [[Cell]] -> [String]
boardToStrings = map rowToString

--Checks if a game board is completed
boardIsDone :: Board -> Bool
boardIsDone [] = True
boardIsDone (clist:rest) = (rowIsDone clist) && (boardIsDone rest)

--Helper for boardIsDone
rowIsDone :: [Cell] -> Bool
rowIsDone [] = True
rowIsDone (cell : rest) = (isDone cell) && (rowIsDone rest)

-- Helper for rowIsDone
isDone :: Cell -> Bool
isDone (Mine, _)       = True
isDone (_, Revealed _) = True
isDone _               = False

printBoard :: Board -> IO ()
printBoard = sequence_ . map putStrLn . addWalls . boardToStrings

-- Helpers for printBoard
createHorizontalWall :: Int -> String
createHorizontalWall n = '+' : replicate n '-' ++ "+"

addVerticalWalls :: String -> String
addVerticalWalls s = '|' : s ++ "|"

addWalls :: [String] -> [String]
addWalls [] = [ "++", "++" ]
addWalls s = end : middle ++ [end]
                where end = createHorizontalWall len
                      len = length $ head s
                      middle = map addVerticalWalls s
                      
                      

getNeighbors :: Row -> Column -> Board -> Int
getNeighbors r c board = length $ neighborcoords r c

getSafe :: row -> col -> board -> Maybe Cell
getSafe r c board = Nothing

neighborcoords :: Num row => Num col => row -> col -> [(row, col)] 
--neighborcoords 0 0 = [(-1,-1), (-1,0),...] 8 things
neighborcoords r c = [(r, c + 1), (r, c - 1),(r + 1, c + 1),(r + 1, c ),(r + 1, c - 1),(r - 1, c - 1),(r - 1, c),(r - 1, c + 1)]

countMines :: [Maybe Cell] -> Int
countMines [Nothing] = 0
countMines (Just c:rest) = 1 + countMines rest




-- String to Act Helper and IO Action

stringAct :: String -> Maybe Act
stringAct "Flag" = Just Flag
stringAct "flag" = Just Flag
stringAct "F" = Just Flag
stringAct "f" = Just Flag
stringAct "Reveal" = Just Reveal
stringAct "reveal" = Just Reveal
stringAct "R" = Just Reveal
stringAct "r" = Just Reveal
stringAct "Question" = Just Question
stringAct "question" = Just Question
stringAct "Q" = Just Question
stringAct "q" = Just Question
stringAct _ = Nothing

makeAction :: IO Action
makeAction = do
   print "Please type desired row:"
   hFlush stdout
   r <- getLine
   let row = read r
   print "Please type desired column:"
   c <- getLine
   let column = read c
   print "Please type desired act:"
   x <- getLine
   case (stringAct x) of
      Just a -> return (row,column,a)
      Nothing -> makeAction
      
-- Build random board with bombs

buildBoard :: RandomGen g => g -> Int -> Int -> Int -> (Board, g)
buildBoard gen n h w = let (bombs, gen') = bombsLocations gen n h w
                       in (boardFromBombList h w bombs, gen')

bombLocation :: RandomGen g => g -> Int -> Int -> ((Row, Column), g)
bombLocation gen numRows numCols = let (r, gen') = randomR (0, numRows - 1) gen
                                       (c, gen'') = randomR (0, numCols - 1) gen'
                                   in ((r, c), gen'')                                       

bombsLocations :: RandomGen g => g -> Int -> Int -> Int -> ([(Row, Column)], g)
bombsLocations gen 0 _ _             = ([], gen)
bombsLocations gen n numRows numCols = let (p, gen') = bombLocation gen numRows numCols
                                           (rest, gen'') = bombsLocations gen' (n - 1) numRows numCols
                                       in if p `elem` rest
                                          then bombsLocations gen'' n numRows numCols
                                          else (p:rest, gen'')

boardFromBombList :: Height -> Width -> [(Row, Column)] -> Board
boardFromBombList h w ps = board h w (\r c -> if (r, c) `elem` ps then Mine else Safe)
