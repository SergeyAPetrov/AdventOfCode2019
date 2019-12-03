module Day3 where

import Lib
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

data Direction = U | R| D | L deriving (Show,Eq)

toDirection 'U' = U
toDirection 'R' = R
toDirection 'D' = D
toDirection 'L' = L

solve1 wire1src wire2src = 
     let pointsWithZero = wireIntersection wire1src wire2src
         points = Set.delete (0,0) pointsWithZero
         dist = Set.map (\(x,y) -> abs x + abs y) points 
     in Set.findMin dist

solve2 wire1src wire2src = 
    let rw1 = reverse $ wire $ parse wire1src
        rw2 = reverse $ wire $ parse wire2src
        intersections = Set.delete (0,0) $ Set.intersection (Set.fromList rw1) (Set.fromList rw2)
        wireWithPos1 = zip rw1 [0..]
        wireWithPos2 = zip rw2 [0..]
        dict1 = Map.fromListWith (\a b -> b) wireWithPos1
        dict2 = Map.fromListWith (\a b -> b) wireWithPos2
        candidates = Set.map (\i -> (dict1 Map.! i + dict2 Map.! i)) intersections
    in 
       Set.findMin candidates

wireIntersection wire1src wire2src = 
    let wire1 = Set.fromList $ wire $ parse wire1src
        wire2 = Set.fromList $ wire $ parse wire2src
        in Set.intersection wire1 wire2

parse :: String -> [(Direction, Int)]
parse wire =
    map parseEntry $ wordsWhen (==',') wire

parseEntry (e:es) = (toDirection e,read es)

wire w = wire' w [(0,0)]

wire' [] r = r
wire' (x:xs) r =
    wire' xs (section ++ r)
    where 
        section = wireStep (head r) x

wireStep (startx,starty) (direction, distance) =
    [(opx startx delta, opy starty delta) | delta <- [distance, distance-1..1]]
    where
        (opx,opy) = getOp direction

getOp U = (const,(+))
getOp R = ((+),const)
getOp D = (const, (-))
getOp L = ((-),const)
