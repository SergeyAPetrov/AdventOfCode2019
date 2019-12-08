module Day8 where

import Data.List
import Data.List.Split

solve1 input =
    count '1' target * count '2' target
    where
        layers = chunksOf (25*6) input
        layersWithZeroCount = map (\l -> (l, count '0' l)) layers
        (target, _) = minimumBy layersComparer layersWithZeroCount

count x xs = length $ filter (==x) xs

layersComparer (_, zeroCount1) (_, zeroCount2)
    | zeroCount1 > zeroCount2 = GT
    | zeroCount1 < zeroCount2 = LT
    | otherwise = EQ

solve2 input =
    chunksOf 25 $ foldl1 foldLayers layers
    where
        layers = chunksOf (25*6) input

foldLayers = zipWith foldPixel

foldPixel '0' _ = '0'
foldPixel '1' _ = '1'
foldPixel _ p = p
