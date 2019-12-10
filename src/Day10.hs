module Day10 where

import Data.List

sameLine (x1, y1) (x2,y2) (x3,y3) = 
    area == 0
    where area = x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)

pointNotVisible (xBase, yBase) (x2,y2) (x3,y3) = 
    sameSign (xBase - x2) (xBase - x3) 
        && sameSign (yBase - y2) (yBase - y3) 

sameSign a b
    | a>=0 && b>=0 = True 
    | a<0 && b<0   = True 
    | otherwise  = False 
