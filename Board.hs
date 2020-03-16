module Board (
   Board,
   Cell,
   board,
   get,
   set
) where

import Types

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