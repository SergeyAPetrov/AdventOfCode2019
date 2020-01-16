module Day16 where

-- n = 1..
pattern n =
    tail $ concat $ repeat nthPattern
    where 
        basePattern = [0,1,0,-1]
        nthPattern = concatMap ((take n).repeat) basePattern