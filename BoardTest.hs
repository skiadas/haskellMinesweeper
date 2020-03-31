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

tests = TestList [
      get 1 2 myBoard ~?= (Mine, Blank),
      get 2 4 myBoard ~?= (Mine, Blank),
      get 0 0 myBoard ~?= (Safe, Blank),
      get 2 0 myBoard ~?= (Safe, Blank),
      get 0 4 myBoard ~?= (Safe, Blank),
      --Testing Set Function
      get 1 2 (set 1 2 Flagged myBoard) ~?= (Mine, Flagged),
      get 2 4 (set 2 4 Questioned myBoard) ~?= (Mine, Questioned),
      get 2 0 (set 2 0 Questioned myBoard) ~?= (Safe, Questioned),
      get 0 4 (set 0 4 Flagged myBoard) ~?= (Safe, Flagged),
      --Testing cellToChar Function
      cellToChar (Mine, Revealed 1) ~?= '\128163',
      cellToChar (Safe, Revealed 1) ~?= '1',
      cellToChar (Mine, Blank) ~?= ' ',
      cellToChar (Safe, Blank) ~?= ' ',
      cellToChar (Mine, Flagged) ~?= '\128681',
      cellToChar (Safe, Flagged) ~?= '\128681',
      cellToChar (Mine, Questioned) ~?= '\63',
      cellToChar (Safe, Questioned) ~?= '\63'
   ]

-- Only use if you want to run the tests in this file only
main = do
   runTestTT tests
   return ()
