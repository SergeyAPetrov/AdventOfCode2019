module Main where

import Day10

main :: IO ()
main = 
    do 
        contents <- lines <$> readFile "input/10.txt"
        --contents <- readFile "input/8.txt"
        let answer = solve contents
        print answer
        --mapM_ putStrLn answer