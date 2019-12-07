module Day6 where

import Data.List
import Data.Tree

getNodes treeData root =
    map (drop prefixLen) $ filter (isPrefixOf (root ++ ")")) treeData
        where prefixLen = length root + 1

buildNode treeData name =
    (name, getNodes treeData name)

buildTree treeData =
    unfoldTree (buildNode treeData)

solve1 treeData =
    let tree = buildTree treeData "COM"
        treeLevels = zip [0..] $ levels tree
    in
        foldr (\(i, level) acc -> acc + (i * length level)) 0 treeLevels

pathFinder target node fchilds =
    if node == target then
        [node]
    else
        case filter (not . null) fchilds of
            [p] -> node : p
            _ -> []

findRootPath tree target =
    foldTree (pathFinder target) tree

solve2 treeData =
    let tree = buildTree treeData "COM"
        p1 = findRootPath tree "YOU"
        p2 = findRootPath tree "SAN"
        in length (p1 \\ p2) + length (p2 \\ p1) - 2