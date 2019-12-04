module Day4 where

import Data.List
import Data.Maybe

solve1 a b =
    length $ filter validPass [a..b]

validPass x = all ($ x) [sameDigits, neverDecrease]

sameDigits x =
    any (\(a,b) -> a==b) pairs
    where 
        pairs = pairwise $ toDigits x

neverDecrease x =
    all (\(a,b) -> a<=b) pairs
    where 
        pairs = pairwise $ toDigits x

toDigits = reverse.unfoldr (\i -> if i == 0 then Nothing else Just (i `mod` 10, i `div` 10))

pairwise xs = zip xs $ tail xs

