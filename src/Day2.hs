module Day2 where

import Lib

solve1 content = 
    step xs 0
    where 
        ws = wordsWhen (==',') content
        xs = map read ws

put xs x idx = take idx xs ++ [x] ++ drop (idx+1) xs

step xs idx =
    if xs !! idx == 99 then
        xs !! 0
    else
        step (step' xs idx) (idx+4)

step' xs idx = 
    put xs newValue newInd
    where 
        value1 = xs !! (xs !! (idx+1))
        value2 = xs !! (xs !! (idx+2))
        newInd = xs !! (idx+3)
        op = case xs !! idx of
                1 -> (+)
                2 -> (*)
        newValue = op value1 value2