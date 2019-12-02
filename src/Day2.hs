module Day2 where

import Lib
import Data.List

parse s =
    map read (wordsWhen (==',') s)

solve1 content =
    head $ Day2.traverse xs
    where
        xs = parse content

solve2 content =
    100*(match !! 1) + (match !! 2)
    where
        xs = parse content
        possibleSolutions = [put a 1 $ put b 2 xs
                | a <- [0..99],b <- [0..99]]
        ass = map Day2.traverse possibleSolutions
        match = head $ filter (\(a:as)->a==19690720) ass

put x n xs = take n xs ++ [x] ++ drop (n+1) xs

traverse xs = step xs 0

step xs idx =
    if xs !! idx == 99 then
        xs
    else
        step (step' xs idx) (idx+4)

step' xs idx =
    put newValue newIdx xs
    where
        value1 = xs !! (xs !! (idx+1))
        value2 = xs !! (xs !! (idx+2))
        newIdx = xs !! (idx+3)
        op = case xs !! idx of
                1 -> (+)
                2 -> (*)
        newValue = op value1 value2