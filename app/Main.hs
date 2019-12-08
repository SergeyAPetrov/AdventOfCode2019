module Main where

import Day8

main :: IO ()
main = 
    do 
        -- contents <- lines <$> readFile "input/8.txt"
        contents <- readFile "input/8.txt"
        let answer = solve2 contents
        --print answer
        mapM_ putStrLn answer