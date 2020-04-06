module BoardTest
(
   tests,
   main
) where

import Test.HUnit
import Board
import Types

f 1 2 = Mine
f 2 4 = Mine
f _ _ = Safe

myBoard = board 3 5 f

--Simple Test Boards for boardIsDone tests
--False Boards
board1F = [[(Safe , Blank) , (Mine, Blank)]]
board2F = [[(Safe , Blank) , (Mine, Blank)] , [(Safe, Revealed 1) , (Mine, Blank)]]
board3F = [[(Safe , Blank) , (Mine, Blank)] , [(Safe, Revealed 1) , (Safe, Revealed 1)] , [(Mine , Blank) , (Mine , Blank)]]
board4F = [[(Mine , Blank) , (Mine , Blank)] , [(Safe , Revealed 1) , (Safe, Revealed 1)] , [(Mine, Blank) , (Safe , Blank)]]
--True Boards
board1T = [[(Safe , Revealed 1) , (Mine, Blank)]]
board2T = [[(Safe , Revealed 1) , (Mine, Blank)], [(Safe, Revealed 1) , (Mine, Blank)]]
board3T = [[(Safe , Revealed 1) , (Mine, Blank)], [(Safe, Revealed 1) , (Safe, Revealed 1)] , [(Mine, Blank) , (Mine , Blank)]]
board4T = [[(Mine , Blank) , (Mine , Blank)] , [(Safe , Revealed 1) , (Safe, Revealed 1)] , [(Mine, Blank) , (Safe , Revealed 1)]]

tests = TestList [
   "myBoard" ~: TestList [
      get 1 2 myBoard ~?= (Mine, Blank),
      get 2 4 myBoard ~?= (Mine, Blank),
      get 0 0 myBoard ~?= (Safe, Blank),
      get 2 0 myBoard ~?= (Safe, Blank),
      get 0 4 myBoard ~?= (Safe, Blank)
      ],
   "set" ~: TestList [
      --Testing Set Function
      get 1 2 (set 1 2 Flagged myBoard) ~?= (Mine, Flagged),
      get 2 4 (set 2 4 Questioned myBoard) ~?= (Mine, Questioned),
      get 2 0 (set 2 0 Questioned myBoard) ~?= (Safe, Questioned),
      get 0 4 (set 0 4 Flagged myBoard) ~?= (Safe, Flagged)
      ],
   "cellToChar" ~: TestList [
      --Testing cellToChar Function
      cellToChar (Mine, Revealed 1) ~?= '\128163',
      cellToChar (Safe, Revealed 1) ~?= '1',
      cellToChar (Mine, Blank) ~?= ' ',
      cellToChar (Safe, Blank) ~?= ' ',
      cellToChar (Mine, Flagged) ~?= '\128681',
      cellToChar (Safe, Flagged) ~?= '\128681',
      cellToChar (Mine, Questioned) ~?= '\63',
      cellToChar (Safe, Questioned) ~?= '\63'
      ],
  "boardIsDone" ~: TestList [
      --Testing boardIsDone on False Boards
      boardIsDone board1F ~?= False,
      boardIsDone board2F ~?= False,
      boardIsDone board3F ~?= False,
      boardIsDone myBoard ~?= False,
      --Testing boardIsDone on True Boards
      boardIsDone board1T ~?= True,
      boardIsDone board2T ~?= True,
      boardIsDone board3T ~?= True,
      boardIsDone board4T ~?= True
      ]
   ]

-- Only use if you want to run the tests in this file only
main = do
   runTestTT tests
   return ()
