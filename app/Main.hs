module Main where

import Day14

main :: IO ()
main = 
    do 
        contents <- lines <$> readFile "input/14.txt"
        --contents <- readFile "input/8.txt"
        let answer = solve contents
              
        print answer 
        --mapM_ putStrLn answer