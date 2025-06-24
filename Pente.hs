module Pente where
import Minmax
import qualified Data.Sequence as Seq
import Data.Foldable
import GHC.Float
import Debug.Trace

-- TODO change board into an ADT which also contains the size and make lookup 2D!

data Player = A | B deriving (Eq, Show)

infinity :: Float
infinity = 1/0

nextPlayer :: Player -> Player
nextPlayer A = B
nextPlayer B = A

playerToTile :: Player -> Tile
playerToTile A = TA
playerToTile B = TB

playerToWin :: Player -> Pente
playerToWin A = WinA
playerToWin B = WinB

data Tile = TA | TB | TEmpty deriving (Eq)

otherTile :: Tile -> Tile
otherTile TA = TB
otherTile TB = TA
otherTile TEmpty = TEmpty

data Pente = 
    WinA | -- If A wins
    WinB | -- If B wins
    Pente -- If neither has won yet
    { currentPlayer :: Player
    , board         :: Seq.Seq Tile -- List of tiles that represents the board. Indexed by multiplication of x and y.
    , pairsA        :: Int -- Amount of pairs that A has collected.
    , pairsB        :: Int -- Amount of pairs that B has collected.
    , sizeX         :: Int
    , sizeY         :: Int
    } deriving (Eq)

-- Lookup relative to the given index
-- x,y, sizeX, sizeY
twoDLookup :: Int -> Int -> Int -> Int -> Seq.Seq a -> Maybe a
twoDLookup = 

{-| Calculate the next state of the board.

Args:
    pente (Pente): Current state
    index (Int): The place where the current player wants to put their tile.
-}
nextPente :: Pente -> Int -> Pente
nextPente (Pente player board pairsA pairsB sizeX sizeY) index = 
    let newBoard = Seq.update index (playerToTile player) board 
        in if (fiveRow index newBoard sizeX sizeY) then (playerToWin player) else
            let (n, newerBoard) = twoPair index newBoard sizeX sizeY in
            if (pairsA + n >= 5) then WinA 
                else let pairsA' = if player == A then pairsA+n else pairsA
                         pairsB' = if player == B then pairsB+n else pairsB in
                    (Pente { currentPlayer = (nextPlayer player), board = newerBoard, pairsA = pairsA', pairsB = pairsB', sizeX = sizeX, sizeY = sizeY })
nextPente x _ = x

-- Check if the index is part of a five row.
fiveRow :: Int -> Seq.Seq Tile -> Int -> Int -> Bool
fiveRow ind board sizeX sizeY = 
    let p i = (Seq.lookup i board == Seq.lookup ind board && (Seq.lookup i board /= Nothing)) in
        all p [ind-4, ind-3, ind-2, ind-1] || -- left
        all p [ind+1, ind+2, ind+3, ind+4] || -- right
        all p [ind-4*sizeX, ind-3*sizeX, ind-2*sizeX, ind-1*sizeX] || -- top
        all p [ind+4*sizeX, ind+3*sizeX, ind+2*sizeX, ind+1*sizeX] || -- bottom
        all p [ind-4*sizeX-4, ind-3*sizeX-3, ind-2*sizeX-2, ind-1*sizeX-1] || -- top left
        all p [ind-4*sizeX+4, ind-3*sizeX+3, ind-2*sizeX+2, ind-1*sizeX+1] || -- top right
        all p [ind+4*sizeX-4, ind+3*sizeX-3, ind+2*sizeX-2, ind+1*sizeX-1] || -- bottom left
        all p [ind+4*sizeX+4, ind+3*sizeX+3, ind+2*sizeX+2, ind+1*sizeX+1] -- bottom right

combine :: (Int, [Int]) -> (Int, [Int]) -> (Int, [Int])
combine (i1, s1) (i2, s2) = (i1 + i2, s1 ++ s2)

emptyTiles :: [Int] -> Seq.Seq Tile -> Seq.Seq Tile
emptyTiles [] board = board
emptyTiles (x:xs) board = emptyTiles xs (Seq.update x TEmpty board)

-- Check how many pairs are stolen by this move and update the board to reflect this.
twoPair :: Int -> Seq.Seq Tile -> Int -> Int -> (Int, Seq.Seq Tile)
twoPair ind board sizeX sizeY = 
    let this = Seq.lookup ind board
        other = fmap otherTile this

        -- Take a target index of the other tile. Returns a list of indices that should be emptied
        tp :: (Int, Int, Int) -> (Int, [Int])
        tp (i1, i2, i3) = if (Seq.lookup i1 board == other && Seq.lookup i2 board == other && Seq.lookup i3 board == this) then
                            (1, [i1, i2]) else (0, [])
        (count, toEmpty) = foldr (\x zr -> combine zr (tp x)) (0, []) [
            (ind-1, ind-2, ind-3), (ind+1, ind+2, ind+3), (ind-1*sizeX, ind-2*sizeX, ind-3*sizeX), (ind+1*sizeX, ind+2*sizeX, ind+3*sizeX),
            (ind-1-1*sizeX, ind-2-2*sizeX, ind-2-2*sizeX),
            (ind+1-1*sizeX, ind+2-2*sizeX, ind+2-2*sizeX), (ind-1+1*sizeX, ind-2+2*sizeX, ind-2+2*sizeX), (ind+1+1*sizeX, ind+2+2*sizeX, ind+2+2*sizeX)]
        in (count, emptyTiles toEmpty board)

instance Minmax Pente where
    nextMoves :: Pente -> [Pente]
    nextMoves pente@(Pente player board pairsA pairsB sizeX sizeY) = [nextPente pente i | i <- [0..(sizeX-1)*(sizeY-1)], board `Seq.index` i == TEmpty]
    nextMoves x = [x]

    heuristic :: Pente -> Float
    heuristic WinA = -infinity
    heuristic WinB = infinity
    heuristic (Pente _ _ pairsA pairsB _ _) = int2Float (pairsA - pairsB)

instance Show Tile where
    show :: Tile -> String
    show tile = case tile of
        TEmpty -> " "
        TA -> "A"
        TB -> "B"


showBoard :: Seq.Seq Tile -> Int -> Int -> String
showBoard board sizeX sizeY = "\n   " ++ foldMap (\x -> show x ++ sp x ++ "|") [0..sizeX-1] ++
    foldMap (\i -> if (mod i sizeX == 0) 
        then "\n" ++ show (i `div` sizeX) ++ sp (i `div` sizeX) ++ "| " ++ show (board `Seq.index` i) 
        else "| " ++ (show (board `Seq.index` i))) [0..(sizeX*sizeY-1)]
    where sp x = if x < 10 then " " else ""

instance Show Pente where
    show :: Pente -> String
    show pente = case pente of
        WinA -> "A wins."
        WinB -> "B wins."
        (Pente player board pairsA pairsB sizeX sizeY) -> 
            "Turn: " ++ show player ++ "\n" ++
            "A Pairs: " ++ show pairsA ++ "\n" ++
            "B Pairs: " ++ show pairsB ++ "\n" ++
            showBoard board sizeX sizeY

newBoard :: Int -> Pente
newBoard n = Pente A  (Seq.replicate (n*n) TEmpty) 0 0 n n