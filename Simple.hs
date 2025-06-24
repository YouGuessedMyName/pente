-- Simple example for testing Minmax
module Simple where
import Minmax

infinity :: Float
infinity = 1/0


data Letter = A | B | C | D | E 
    deriving (Show, Eq)

instance Minmax Letter where
    nextMoves :: Letter -> [Letter]
    nextMoves l = case l of
        A -> [B, C]
        B -> [D, D]
        C -> [D, E]
        _ -> []

    heuristic :: Letter -> Float
    heuristic l = case l of
        A -> 0
        B -> 0
        C -> 0
        D -> -infinity
        E -> 1