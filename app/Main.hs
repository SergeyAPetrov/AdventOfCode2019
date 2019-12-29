module Main where

import Day12

main :: IO ()
main = 
    do 
        --contents <- lines <$> readFile "input/10.txt"
        --contents <- readFile "input/8.txt"
        let answer = solve1 [Si (Pt (-16) 15 (-9)) (Vl 0 0 0),
                                Si (Pt (-14) 5 4) (Vl  0 0 0),
                                Si (Pt 2 0 6) (Vl  0 0 0),
                                Si (Pt (-3) 18 9) (Vl 0 0 0)] 1000
        print answer
        --mapM_ putStrLn answer