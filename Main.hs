module Main (main) where

import System.Environment (getArgs, getProgName)
import Data.Char (toLower)
import Board

--starts the game by getting user input for columns rows and bombs or sets default value if no input is given or if invalid input is entered.
startGame :: IO Board
startGame = do
    inputCol <- promptInt "Please enter how many columns you want for your game board:  " 5
    inputRows <- promptInt "Please enter how many rows you want for your game board:  " 5
    inputBombs <- promptInt "Please enter the number of bombs you would like:  " 7
    func <- boardWithBombs inputRows inputCol inputBombs
    return $ board inputRows inputCol func
            
playGame :: Board -> IO ()
playGame _ = return ()

promptInt :: String -> Int -> IO Int
promptInt s n = do 
   userInput <- prompt s
   if userInput == ""
       then return n
       else return (read userInput)

prompt :: String -> IO String
prompt s = do
    putStr s
    input <- getLine
    return input

main :: IO ()
main = do
   wantsToPlay <- map toLower <$> prompt "Hello and welcome to Minesweeper.\nWould you like to play? "
   if wantsToPlay == "yes" || wantsToPlay == "y"
      then startGame >>= playGame
      else return ()
   putStrLn "Good Bye!"
