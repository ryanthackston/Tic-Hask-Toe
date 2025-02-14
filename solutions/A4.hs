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

isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ _ [] = False
isWinningLine_ p l | (replicate 3 X /= l) = False 
isWinningLine_ p l 
  | head (filter (== replicate 3 p) [l]) == l = True

-- *** Assignment 4-2 *** --

-- Q#07

testXDiag = [[X,O,O], [X,X,O],[O,O,X]]

-- isWinningLine :: Player -> Line -> Bool
isWinningLine' :: Player -> Line -> Bool
isWinningLine' p xs  = foldr (\x y -> y || x == p) False xs
--  where
--    reducer p b = b || x == p
-- isWinningLine p b  = foldr (\x b -> (==) (replicate 3 x) (getAllLines)  (X) (getAllLines testXDiag)

{- elem_ :: forall a. Eq a => a -> [a] -> Bool
elem_ q xs = foldr (\x b -> b || x == q) False xs
    where
        reducer :: Eq a => a -> Bool -> Bool
        reducer x b = b || x == q -}

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

{- prependRowIndices' :: [String] -> [String]
prependRowIndices' []       = []
prependRowIndices' (x:xs) = go (indexRowStrings (x:xs))
  where
    go :: [(Char, String)] -> [String]
    go []                = []
    go ((c, rs):rss) = (c : rs) : go rss -}

prependRowIndices :: [String] -> [String]
prependRowIndices s = zipWith (:) ['A'..] s

-- Q#11

formatBoard :: Board -> String
formatBoard b = unlines(_HEADER_ : prependRowIndices(formatRows b))