module Day14 where

import Data.List
import Data.List.Split
import qualified Data.Graph as G
import Data.Function (on)

data Reagent = Reagent { amount :: Integer, name :: String}
    deriving (Eq, Show, Read)

data Reaction = Reaction { reagents :: [Reagent], result :: Reagent}
    deriving (Eq, Show, Read)

parse lines =
    map parseLine lines

parseLine line =
    Reaction reagents result
    where
        parts = splitOn " => " line
        reagents = map parseReagent $ splitOn ", " $ head parts
        result = parseReagent $ parts!!1

parseReagent str =
    Reagent amount name
    where
        parts = splitOn " " str
        amount = read $ head parts
        name = parts!!1

reactionsInOrder reactions =
    reverse $ map (\(x,_,_) -> x) $ map vertexToNode $ G.topSort graph
    where
        (graph, vertexToNode, _) = G.graphFromEdges (("FUEL", "FUEL", []):graphNodes)
        graphNodes = map edgeListToNodeTuple groupedEdges
        edgeListToNodeTuple edges = 
            (fst $ head edges, fst $ head edges, map snd edges)
        groupedEdges = groupBy ((==) `on` fst) $ sortOn fst edges
        edges = concatMap reactionToEdge reactions
        reactionToEdge reaction = 
            map (\r-> (name r, name $ result reaction)) $ reagents reaction

reactionsToBackedges reactions = 
    edges
    where
        edges = concatMap reactionToEdge reactions
        reactionToEdge reaction = map (\r-> (name $ result reaction, amount $ result reaction, name r, amount r)) $ reagents reaction

solve1 lines = 
    foldl solutionFolder' initialValue orderedNodes
    where 
        reactions = parse lines
        orderedNodes = reactionsInOrder reactions
        reactionData = reactionsToBackedges reactions
        solutionFolder' = solutionFolder reactionData
        initialValue = zip orderedNodes (1:repeat 0)

-- reaction data result element goes first
solutionFolder :: [(String, Integer, String, Integer)] -> [(String, Integer)] -> String -> [(String, Integer)]
solutionFolder reactionData accumulator currentNode =
    map processNode accumulator
    where
        outboundEdges = filter (\(x,_,_,_)-> x == currentNode) reactionData
        currentNodeAmount = snd $ head $ filter (\(name,_ ) -> name == currentNode) accumulator
        processNode (node, amount) = 
            if (length edges) == 0 then
                (node,amount)
            else
                let 
                    (resultName,resultAmount,sourceName,sourceAmount) = head edges
                    coef = (fromIntegral currentNodeAmount)/(fromIntegral resultAmount)
                    delta = (ceiling coef)*sourceAmount
                in (node, amount + delta)
            where 
                edges = filter (\(_,_,x,_) -> x==node) outboundEdges

-- (myGraph,vertexToNode,keyToVertex) = G.graphFromEdges [
--       ("node5","5",["2","0"]),     -- the first component can be of any type
--       ("node0","0",[]),
--       ("node2","2",["3"]),
--       ("node3","3",["1"]),
--       ("node1","1",[]),
--       ("node4","4",["0","1"])
--    ]

-- sorted = map vertexToNode $ G.topSort myGraph