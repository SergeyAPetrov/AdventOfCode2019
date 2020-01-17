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

calculateElement xs pattern = 
    (abs total) `mod` 10
    where 
        total = sum $ map (uncurry (*)) $ zip xs pattern

solve input = 
    take 100 $ map (concat.(map show)) phases
    where
        inputSignal = parse input
        phases = iterate phase inputSignal
        patterns = map pattern [1..(length inputSignal)] 
        phase xs = 
            map (uncurry calculateElement) $ map ((,) xs) patterns