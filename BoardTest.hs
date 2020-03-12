module BoardTest
(
   runTests
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
      get 0 4 myBoard ~?= (Safe, Blank)
   ]

runTests = do runTestTT tests
              return ()
