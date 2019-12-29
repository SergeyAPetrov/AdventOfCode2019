module Day12 where

import Data.List

data Point a = Pt a a a deriving (Eq, Show)
data Velocity a = Vl a a a deriving (Eq, Show)
data SpeedInfo a = Si (Point a) (Velocity a) deriving (Eq, Show)

getPoint (Si pt _) = pt

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
