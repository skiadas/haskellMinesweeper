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
   stringAct
) where

import Types
import Data.Char
import System.IO

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
                      
                      
getNeighbors :: Row -> Col -> Board -> Int
getNeighbors r c board = 0

getSafe :: row -> col -> board -> Maybe Cell
getSafe r c board = Nothing

neighborcoords :: row -> col -> [(row, col)], 
--neighborcoords 0 0 = [(-1,-1), (-1,0),...] 8 things
neighborcoords _ _ = []

countMines :: [Maybe Cell] -> Int
countMines c = 0


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
                      
