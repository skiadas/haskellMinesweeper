module Main (main) where

import System.Environment (getArgs, getProgName)
import qualified BoardTest

main :: IO ()
main = do
   args <- getArgs
   case args of
      ("tests" : _) -> BoardTest.runTests
      _ -> do
         name <- getProgName
         putStrLn ("try './" ++ name ++ " tests' to run tests")