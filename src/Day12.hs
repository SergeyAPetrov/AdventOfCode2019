module Day12 where

import Data.List

data Point a = Pt a a a deriving (Eq, Show)
data Velocity a = Vl a a a deriving (Eq, Show)
data SpeedInfo a = Si (Point a) (Velocity a) deriving (Eq, Show)

getPoint (Si pt _) = pt

toList (Si (Pt x y z) (Vl x' y' z')) =
    [x,y,z]

applyVelocity (Si (Pt x y z) (Vl x' y' z')) =
    Si (Pt (x+x') (y+y') (z+z')) (Vl x' y' z')

applyGravity (Si (Pt x1 y1 z1) (Vl vx vy vz)) (Pt x2 y2 z2) =
    Si (Pt x1 y1 z1) (Vl (gravityCalc vx x1 x2) (gravityCalc vy y1 y2) (gravityCalc vz z1 z2))
    where
        gravityCalc v a b
            | a < b = v+1
            | a > b = v-1
            | otherwise = v

applyGravities si pts =
    foldl applyGravity si pts

step sis =
    map planetMover sis
    where
        planetMover si =
            applyVelocity $ applyGravities si sis'
            where sis' = map getPoint $ filter (/=si) sis

totalEnergy sis =
    sum energies
    where
        energies = map moonEnergy sis
        moonEnergy (Si (Pt x y z) (Vl vx vy vz)) =
            potential * kinetic
            where
                potential = abs x + abs y + abs z
                kinetic = abs vx + abs vy + abs vz

solve1 sis n =
    totalEnergy $ steps!!n
    where
        steps = iterate step sis

test sis =
    xs
    where
        steps = iterate step sis
        xs = map getCoord $ map (!!0) steps
        getCoord (Si (Pt x y z) (Vl vx vy vz)) = vz

getPeriod xs =
    snd $ head $ filter fst $ zip periodBitMap [2..]
    where
        periodBitMap = map tryPeriod [2..]
        tryPeriod n =
            l1 == l2
            where
                l1 = take n xs
                l2 = take n $ drop n xs

getPeriods xss =
    map (getPeriod . extractor) [0..n-1]
    where
        n = length $ head xss
        extractor i = map (!!i) xss

lcms xs = foldl1 lcm xs

solve2 sis =
    lcms periods
    where
        steps = iterate step sis
        lines = map (concatMap toList) steps
        periods = getPeriods lines