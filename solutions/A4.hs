module A4 where

import A1
import A2
import A3 hiding (
  _HEADER_,
  showSquares,
  dropFirstCol,
  dropLastCol,
  formatRows,
  isWinningLine,
  prependRowIndices
  )

-- *** Assignment 4-1 *** --

-- Q#01
_HEADER_ :: String
_HEADER_ = ' ' : formatLine(map show _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares r = map show r

-- Q#03

dropFirstCol :: Board -> Board
dropFirstCol b = map (drop 1) b

-- Q#04

dropLastCol :: Board -> Board
dropLastCol b = map (take (length b - 1)) b

--Q#05

formatRows :: [Row] -> [String]
--formatRows b = map formatLine (map (showSquares) b)
formatRows b = map (formatLine . showSquares) b

-- Q#06

isWinningLine :: Player -> Line -> Bool
isWinningLine_ p b = filter (== replicate 3 p) (getAllLines b)

-- *** Assignment 4-2 *** --

-- Q#07

isWinningLine' :: Player -> Line -> Bool
isWinningLine' p xs  = foldr (\x y -> y || x == p) False xs

-- Q#08

_X_WIN_ = [ [X, O, O]
              , [O, X, O]
              , [O, O, X]
              ]

_O_WIN_ = [ [O, X, O]
              , [X, X, O]
              , [X, O, O]
              ]

hasWon :: Player -> Board -> Bool
hasWon p b = foldr (\x y -> y || x == (replicate 3 p) ) False (getAllLines b)


-- Q#09

getGameState :: Board -> GameState
getGameState b
  | hasWon X b = XWon
  | hasWon O b = OWon
  -- Any empty tile means game in progress
  | foldr (\x y -> y || any (==Empty) x) False (getAllLines b) = InProgress
  | otherwise = Tie


playMove :: Player -> Board -> Move -> (GameState, Board)
playMove p b (r,c) = (getGameState(putSquare p b (r,c)), putSquare p b (r,c))

-- Q#10

prependRowIndices = undefined

-- Q#11

formatBoard = undefined
