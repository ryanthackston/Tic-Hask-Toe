module A3 where

import A1
import A2

import Data.Char (ord, toUpper)
import Data.List (transpose,intercalate)
import Data.Bool (bool)

-- *** Assignment 3-1 ***

-- Q#01
showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs

_HEADER_ :: String
_HEADER_ = ' ' : formatLine(showInts _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares (x:xs) = showSquare x : showSquares xs
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
t = _TIED_BOARD_
t2 = [[O, X, X], [X, X, O], [O, O, X]]
e = _EMPTY_BOARD_

dropFirstCol :: Board -> Board
dropFirstCol [] = []
dropFirstCol (x:xs)  = drop 1 x : dropFirstCol xs

dropLastCol :: Board -> Board
dropLastCol [] = []
dropLastCol (x:xs) = take (length x - 1) x : dropLastCol xs

-- Q#06
getDiag1 :: Board -> Line
getDiag1 []      = []
getDiag1 (x:xs) =  head x : getDiag1 (dropFirstCol xs)

getDiag2 :: Board -> Line
getDiag2 []     = []
getDiag2 (x:xs) = last x : getDiag2 (dropLastCol xs)

getAllLines :: Board -> [Line]
--getAllLines b = b ++ (transpose b) ++ [getDiag1 b] ++ [getDiag2 b]

getAllLines b = concat [b, transpose b, [getDiag1 b, getDiag2 b]]
-- Whenever writing a 2nd append ++ , replace the ++ with concat 
-- *** Assignment 3-2 ***

 -- Q#07
putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []
putSquare p (r:rs) (0, j) = let r' = replaceSquareInRow p j r in r':rs
putSquare p b@(r:rs) m@(i, j)
  | i > 0 = r : putSquare p rs (i-1, j)
  | otherwise = b

putSquare' :: Player -> Board -> Move -> Board
putSquare' _ [] _  = []
putSquare' p b (0, c) = replaceSquareInRow p c (head(snd(splitAt 0 b))) : tail(drop 0 b)
putSquare' p b (r, c) 
  | r == 1 = head(take r b) : replaceSquareInRow p c (head(drop r b)) :  tail(drop r b)
  | r == 2 =   [head (take r b), head (tail (take r b)), replaceSquareInRow p c (b !! r )]
  | otherwise = []
-- cutout the desired coordinate, add the Player value, rebuild into new board
{- putSquare p b m = go i
    where
      go i
        | go 1 = replaceSquareInRow p (snd m - 1) (head(fst(splitAt 2 b))) : (splitAt (fst m) b)
        | go 2 = replaceSquareInRow p (snd m - 1) (head(tail(fst(splitAt 2 t))))
        | otherwise = [] -}

{- --Row
head(tail (fst (splitAt 2 t)))
-- Column
fst (splitAt (1) (head(tail (fst (splitAt 2 t)))))
 -}

-- Q#08
-- test = [".SOME", ".TEST", ".STRING"]
-- z = zip ['A'..'C'] test
prependRowIndices :: [String] -> [String]
prependRowIndices []       = []
prependRowIndices (x:xs) = go (indexRowStrings (x:xs))
  where
    go :: [(Char, String)] -> [String]
    go []       = []
    -- go (x:xs) = uncurry (:) x : go xs
prependRowIndices' :: [String] -> [String]
prependRowIndices' []       = []
prependRowIndices' (x:xs) = go (indexRowStrings (x:xs))
  where
    go :: [(Char, String)] -> [String]
    go []                = []
    go ((c, rs):rss) = (c : rs) : go rss

-- Q#09
testX = [X,X,X]
testX2 = [X,X,O]
isWinningLine :: Player -> Line -> Bool
isWinningLine p [] = True
isWinningLine p (r:rs) = r == p && isWinningLine p rs
{- isWinningLine p rs = go False rs
  where
    go p [] = p
    go p (r:rs) = r == p && go True rs
 -}
-- Q#10

isValidMove :: Board -> Move -> Bool
isValidMove b (i, j) = isMoveInBounds (i, j) && go r (i, j)
  where
    r =  head( drop i b)
    go  r (i, 0) = head r == Empty
    go  r (i, j)  = go (drop 1 r) (i, j-1)
