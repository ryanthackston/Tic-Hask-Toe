module Main where

import A1
import A2
import A3
import A4
import A5

main :: IO ()
main = (>>= playThis) firstPlayer
    where
    playThis :: Player -> IO ()
    playThis p = (play _EMPTY_BOARD_) p
