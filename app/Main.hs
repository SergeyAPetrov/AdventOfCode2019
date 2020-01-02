module Main where

import Day12

main :: IO ()
main = 
    do 
        --contents <- lines <$> readFile "input/10.txt"
        --contents <- readFile "input/8.txt"
        -- let p0 = Pt (-1) 0 2
        -- let p1 = Pt 2 (-10) (-7)
        -- let p2 = Pt 4 (-8) 8
        -- let p3 = Pt 3 5 (-1)
        -- let v0 = Vl 0 0 0  
        -- let answer = solve2 [Si p0 v0, Si p1 v0, Si p2 v0, Si p3 v0] 

        let answer = solve2 [Si (Pt (-16) 15 (-9)) (Vl 0 0 0),
                                Si (Pt (-14) 5 4) (Vl  0 0 0),
                                Si (Pt 2 0 6) (Vl  0 0 0),
                                Si (Pt (-3) 18 9) (Vl 0 0 0)]
                      
        print answer
        --mapM_ putStrLn answer