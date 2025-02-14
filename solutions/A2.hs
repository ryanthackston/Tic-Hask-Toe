{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.Char (ord, toUpper)
import Data.List (intercalate, transpose)

-- *** Assignment 2-1 *** --

-- Q#01

promptPlayer :: Player -> String
promptPlayer Empty = concat ["No player's turn" ]
promptPlayer p = concat ["Player ", show(p), "'s turn: enter a row and column position " ]

-- Q#02
_RANGE_ ::  [Int]
_RANGE_ = [0 .. ((_SIZE_) - 1)]

-- Q#03
isDigit :: Char -> Bool
isDigit c = c `elem` ['0' .. '9']


readDigit :: Char -> Int
readDigit c = if isDigit c then read [c] else -1

-- Q#04
-- data Square = X | O | Empty deriving(Show, Eq)

_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ Empty

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

isTied :: Board -> Bool
isTied b = not(elem Empty (concat b))

_TIED_BOARD_ :: Board 
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

-- Q#06

indexRowStrings :: [String] -> [(Char,String)]
indexRowStrings []  = []
indexRowStrings s = zip ['A'.. ] s

-- Q#07

formatLine :: [String] -> String
formatLine s = _SEP_ ++ intercalate _SEP_ s ++ _SEP_

-- *** Assignment 2-2 *** --

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds (x, y) = checkX && checkY
  where
    checkX = (x >= 0) && (x < _SIZE_)
    checkY = (y >= 0) && (y < _SIZE_)

-- Q#09

stringToMove :: String -> Move
stringToMove [r, c] = (convertRowIndex r - 1, readDigit c)
stringToMove [r, c] = (readDigit r,  readDigit c)
stringToMove [r, c] = ( ((convertRowIndex r) - 1), ((convertRowIndex c) - 1) )
stringToMove _      = _INVALID_MOVE_

{- stringToMove :: String -> Move
stringToMove [] = _INVALID_MOVE_
stringToMove [_] = _INVALID_MOVE_
stringToMove [x,y] = let
  xMove = if convertRowIndex x > readDigit x 
    then convertRowIndex x
    else readDigit x
  yMove = if (convertRowIndex y) > (readDigit y) 
    then convertRowIndex y
    else readDigit y
  in (xMove, yMove)
stringToMove [_:_] = _INVALID_MOVE_ -}
  
-- Q#10
--replaceSquareInRow :: Player -> Int -> Row -> Row
--replaceSquareInRow P C R = 
--  let newRow = if R == _EMPTY_ROW_ 
--    then tail(fst(splitAt C (_EMPTY_ROW_))) ++ [X] ++ (snd(splitAt C (_EMPTY_ROW_ ))) 
--    else 
--  in 

{- _TIED_BOARD_ :: Board
_TIED_BOARD_ = [[X, O, O], [O, X, X], [O, X, O]] -}

{- t = last _TIED_BOARD_ -}

-- Q#10

{- replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow p c r = let
  rowReplaceByCol = case c of 
    1 -> [p] : snd splitAt c r
    2 -> [head fst(splitAt c r)] ++ [p] ++ snd splitAt c r
    3 -> fst splitAt (c-1) r ++ [p]
    _ -> r
  in rowReplaceByCol -}

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow p c r = xs ++ ys'
  where
    (xs, ys) = splitAt c r
    ys'
      | null ys = []
      | c < 0     = ys
      | otherwise = p : tail ys