module Main where

import Day4

main :: IO ()
main = 
    do 
        --contents <- lines <$> readFile "input/3.txt"
        -- contents <- readFile "input/2.txt"
        let answer = solve1 152085 670283
        print answer