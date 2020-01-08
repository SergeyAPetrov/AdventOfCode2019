module Day14 where

import Data.List
import Data.List.Split

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