-- General minmax implementation.
module Minmax where
import Data.List

argmax :: Ord b => (a -> b) -> [a] -> a
argmax f xs = maximumBy (\x y -> compare (f x) (f y)) xs

argmin :: Ord b => (a -> b) -> [a] -> a
argmin f xs = minimumBy (\x y -> compare (f x) (f y)) xs

class (Eq a) => Minmax a where
    nextMoves :: a -> [a]
    heuristic :: a -> Float

{-| Solve a minmax problem using DFS. 

Args:
    * problem a (Minmax a): The problem to solve.
    * depth (Int): The amount of turns to look ahead.
Returns the board after the resulting move. -}
solve :: (Minmax a) => a -> Int -> a
solve problem depth =
    argmax (\x -> solveMin x (depth-1)) (nextMoves problem)

solveMax :: (Minmax a) => a -> Int -> Float
solveMax problem depth = if (depth <= 0) then (heuristic problem) 
    else maximum (map (\x -> solveMin x (depth -1 )) (nextMoves problem))

solveMin :: (Minmax a) => a -> Int -> Float
solveMin problem depth = if (depth <= 0) then (heuristic problem) 
    else minimum (map (\x -> solveMax x depth) (nextMoves problem))