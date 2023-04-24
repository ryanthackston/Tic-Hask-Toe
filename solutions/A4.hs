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

isWinningLine = undefined

-- Q#08

hasWon = undefined

-- Q#09

getGameState = undefined


playMove = undefined

-- Q#10

prependRowIndices = undefined

-- Q#11

formatBoard = undefined
