module Main where

import Day3

main :: IO ()
main = 
    do 
        contents <- lines <$> readFile "input/3.txt"
        -- contents <- readFile "input/2.txt"
        let answer = solve2 (contents!!0) (contents!!1)
        print answer