module Tests (
   main
) where

import Test.HUnit
import qualified BoardTest

-- To add your tests, import the module above and add a new entry
-- in the list `tests`
tests = TestList [
   "board" ~: BoardTest.tests
   ]


main :: IO ()
main = do
   runTestTT tests
   return ()
