module Main where

import Day2

main :: IO ()
main = 
    do 
        -- contents <- lines <$> readFile "input/2.txt"
        contents <- readFile "input/2.txt"
        let answer = solve2 contents
        print answer