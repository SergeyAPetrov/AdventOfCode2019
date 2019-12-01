module Main where

import Day1

main :: IO ()
main = 
    do 
        contents <- lines <$> readFile "input/1.txt"
        let answer = solve2 contents
        print answer