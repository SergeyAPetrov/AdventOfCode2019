module Day16 where

import Data.Char(digitToInt)

--parse :: [Char] -> [Integer]
parse str = 
    map digitToInt str

-- n = 1..
pattern n =
    tail $ concat $ repeat nthPattern
    where 
        basePattern = [0,1,0,-1]
        nthPattern = concatMap ((take n).repeat) basePattern

--phase :: [Integer] -> [Integer]
phase xs = 
    map (uncurry calculateElement) $ map ((,) xs) $ map pattern [1..(length xs)] 

calculateElement xs pattern = 
    (abs total) `mod` 10
    where 
        total = sum $ map (uncurry (*)) $ zip xs pattern

solve input = 
    take 8 $ phases!!100
    where
        inputSignal = parse input
        phases = iterate phase inputSignal