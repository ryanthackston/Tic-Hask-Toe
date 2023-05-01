module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)

-- *** Assignment 5-1 *** --

-- Q#01

printBoard :: Board -> IO ()
printBoard b = putStrLn $ formatBoard b

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"

printLogo :: IO String
printLogo = readFile (_LOGO_PATH_)

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Square
firstPlayer  = _RANDOM_BOOL_ >>= (\i -> if i==True then return X else return O)

-- Q#04

getMove :: Board -> IO ()
getMove b = do 
    s <- getLine
    print $ isValidMove b  (stringToMove s)
    
-- Q#05

play :: Player -> Board -> IO ()
play = undefined

-- *** Assignment 5-2 *** --

-- Q#07

printLogoDo = undefined

-- Q#08

firstPlayerDo = undefined

-- Q#09

getMoveDo = undefined

-- Q#10

playDo = undefined
