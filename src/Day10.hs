module Day10 where

import Data.List

sameLine (x1, y1) (x2,y2) (x3,y3) =
    area == 0
    where area = x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)

pointNotVisible (xBase, yBase) (x2,y2) (x3,y3) =
    sameSign dx1 dx2     && sameSign dy1 dy2
    && abs dx2 >= abs dx1 && abs dy2 >= abs dy1
        where
            dx1 = xBase - x2
            dx2 = xBase - x3
            dy1 = yBase - y2
            dy2 = yBase - y3

sameSign a b
    | a>=0 && b>=0 = True
    | a<0 && b<0   = True
    | otherwise  = False

pointDominated (xBase,yBase) (x2,y2) (x3,y3) =
    sameLine (xBase,yBase) (x2,y2) (x3,y3) && pointNotVisible (xBase,yBase) (x2,y2) (x3,y3)

pointDominatedBySet (xBase,yBase) points (x3,y3) =
    any (\p -> pointDominated (xBase,yBase) p (x3,y3)) points

setNotDominatedByPoint (xBase, yBase) (x,y) points =
    filter (not . pointDominated (xBase, yBase) (x,y)) points

calculateVisiblePoints (xBase, yBase) points = 
    foldl folder [] points
        where folder = visibilityFolder (xBase, yBase)

-- visibilityFolder :: (Integer, Integer) -> (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)] 
visibilityFolder (xBase, yBase) currentVisiplePoints (x,y) = 
    if pointDominatedBySet (xBase,yBase) currentVisiplePoints (x,y) then
        currentVisiplePoints
    else 
        (x,y) : setNotDominatedByPoint (xBase, yBase) (x,y) currentVisiplePoints