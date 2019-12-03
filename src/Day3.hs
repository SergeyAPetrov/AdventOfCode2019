module Day3 where

import Lib
import Data.List

data Direction = U | R| D | L deriving (Show,Eq)

toDirection 'U' = U
toDirection 'R' = R
toDirection 'D' = D
toDirection 'L' = L

solve1 wire1src wire2src = 
     let pointsWithZero = wireIntersection wire1src wire2src
         points = delete (0,0) pointsWithZero
         dist = map (\(x,y) -> abs x + abs y) points 
     in minimum dist

wireIntersection wire1src wire2src = 
    let wire1 = wire $ parse wire1src
        wire2 = wire $ parse wire2src
        in intersect wire1 wire2

parse :: String -> [(Direction, Int)]
parse wire =
    map parseEntry $ wordsWhen (==',') wire

parseEntry (e:es) = (toDirection e,read es)

wire w = wire' w [(0,0)]

wire' [] r = r
wire' (x:xs) r =
    wire' xs (r ++ section)
    where 
        section = wireStep (last r) x

wireStep (startx,starty) (direction, distance) =
    [(opx startx delta, opy starty delta) | delta <- [1..distance]]
    where
        (opx,opy) = getOp direction

getOp U = (const,(+))
getOp R = ((+),const)
getOp D = (const, (-))
getOp L = ((-),const)
