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

{- getMove :: Board -> IO Move
getMove b = do 
    s <- getLine
    if isValidMove b (stringToMove s) == True then return (stringToMove s) else error("Invalid move! Try again.") >> (getMove b)
 -}
getMove :: Board -> IO Move
getMove b = getLine >>= worker . stringToMove
    where
        worker :: Move -> IO Move
        worker m = if isValidMove b m
                      then return m
                      else putStrLn "Invalid move! Try again" >> getMove b
-- Q#05

play :: Board -> Player -> IO ()
play b p = when _DISPLAY_LOGO_ (printLogo >>= putStrLn)  >> printBoard b >> putStrLn (promptPlayer p) >> getMove b >>= executeMove
    where
        executeMove :: Move -> IO ()
        executeMove m = let (newState, newBoard) = playMove p b m
            in case newState of
                InProgress -> play newBoard (switchPlayer p)
                otherwise  -> printBoard newBoard >> putStrLn (showGameState newState)

{- >> playMove p b (getMove b)
 -}
    --m <- getLine >> playMove p b m 

-- *** Assignment 5-2 *** --

-- Q#07

printLogoDo :: IO String
printLogoDo = do
    readFile (_LOGO_PATH_)

-- Q#08

firstPlayerDo :: IO Square
firstPlayerDo  = do
    x <- _RANDOM_BOOL_
    (\xa -> if xa==True then return X else return O) x

-- Q#09

getMoveDo :: Board -> IO Move
getMoveDo b = do
    m <- getLine
    tmp <- return (stringToMove m)
    if isValidMove b tmp
        then do
            return tmp
        else do
            putStrLn "Invalid move! Try again" 
            getMoveDo b
    



{- getMove :: Board -> IO Move
getMove b = getLine >>= worker . stringToMove
    where
        worker :: Move -> IO Move
        worker m = if isValidMove b m
                      then return m
                      else putStrLn "Invalid move! Try again" >> getMove b -}
-- Q#10

{- playDo :: Board -> Player -> IO ()
playDo b p = do
    when _DISPLAY_LOGO_ (printLogo putStrLn)
    printBoard b
    promptPlayer p
    m <- getMove b
    let (newState, newBoard) = playMove p b m
    case newState do
        InProgress -> play newBoard (switchPlayer p)
        otherwise -> printBoard newBoard putStrLn (showGameState newState) -}

    {- executeMove :: Move -> IO ()
    executeMove m = in case do
        play (playMove p b m) (switchPlayer p)
        showGameState (showgameState (printBoard (playMove p b m)))
 -}
    {- let (newState, newBoard) = playMove p b m
    case newState do
        play newBoard (switchPlayer p)
        printBoard newBoard putStrLn (showGameState newState)
 -}
{- play :: Board -> Player -> IO ()
play b p = when _DISPLAY_LOGO_ (printLogo >>= putStrLn)  >> printBoard b >> putStrLn (promptPlayer p) >> getMove b >>= executeMove
    where
        executeMove :: Move -> IO ()
        executeMove m = let (newState, newBoard) = playMove p b m
            in case newState of
                InProgress -> play newBoard (switchPlayer p)
                otherwise  -> printBoard newBoard >> putStrLn (showGameState newState) -}