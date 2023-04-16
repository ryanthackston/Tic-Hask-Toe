module A3 where

import A1
import A2

import Data.List (transpose)

-- *** Assignment 3-1 ***

-- Q#01
showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs

_HEADER_ :: String
_HEADER_ = tail $ tail $ tail (formatLine([[' ']] ++ showInts _RANGE_))

-- Q#02

showSquares :: [Square] -> [String]
showSquares (x:xs) = show x : showSquares xs
showSquares [] = []


-- Q#03

formatRows :: [Row] -> [String]
formatRows []       = []
formatRows (x:xs) = formatLine (showSquares x) : formatRows xs

-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty [] _            = False
isColEmpty r c             = isMoveInBounds (length r - 1, c) && (r !! c) == Empty

-- Q#05

dropFirstCol :: Board -> Board
dropFirstCol [] = []
dropFirstCol (x:xs)  = drop 1 x : dropFirstCol xs

dropLastCol :: Board -> Board
dropLastCol [] = []
dropLastCol (x:xs)  = take 2 x : dropLastCol xs

-- Q#06

getDiag1 = undefined


getDiag2 = undefined


getAllLines = undefined

-- *** Assignment 3-2 ***

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine = undefined

-- Q#10

isValidMove = undefined
