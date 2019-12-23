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

pointDominated pBase point2 point3 =
    sameLine pBase point2 point3 && pointNotVisible pBase point2 point3

pointDominatedBySet pBase points point3 =
    any (\p -> pointDominated pBase p point3) points

setNotDominatedByPoint pBase point points =
    filter (not . pointDominated pBase point) points

-- visibilityFolder :: (Integer, Integer) -> (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)] 
visibilityFolder pBase currentVisiplePoints point = 
    if pointDominatedBySet pBase currentVisiplePoints point then
        currentVisiplePoints
    else 
        point : setNotDominatedByPoint pBase point currentVisiplePoints

calculateVisiblePoints pBase points = 
    foldl folder [] points
        where folder = visibilityFolder pBase

calculateVisiblePoints' points pBase = 
    calculateVisiblePoints pBase points'
        where points' = filter (/=pBase) points

--solve :: [(Integer, Integer)] -> Integer
solve1 points =
    let visiblePointsPerPoint = map (calculateVisiblePoints' points) points 
        visiblePointsSizePerPoint = map length visiblePointsPerPoint
        in maximum visiblePointsSizePerPoint


--parseInput :: [String] -> [(Integer,Integer)]
parseInput rows =
    let rowsWithCoords = zip [0..] $ map (zip [0..]) rows
    in [ (x,y) | (y,row) <- rowsWithCoords, (x,c) <- row, c == '#']

solve = solve1.parseInput