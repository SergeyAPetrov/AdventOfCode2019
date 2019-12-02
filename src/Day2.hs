module Day2 where

import Lib
import Data.List

solve1 content = 
    step xs 0
    where 
        ws = wordsWhen (==',') content
        xs = map read ws

solve2 content = 
    100*(match !! 1) + (match !! 2)
    where 
        ws = wordsWhen (==',') content
        xs = map read ws
        xss = [(put (put xs a 1) b 2) 
                | a <- [0..99],
                  b <- [0..99]]
        ass = map (\list -> step2 list 0) xss 
        matches = filter (\list->(head list)==19690720) ass
        match = head matches

put xs x idx = take idx xs ++ [x] ++ drop (idx+1) xs

step xs idx =
    if xs !! idx == 99 then
        xs !! 0
    else
        step (step' xs idx) (idx+4)

step2 xs idx =
    if xs !! idx == 99 then
        xs
    else
        step2 (step' xs idx) (idx+4)

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