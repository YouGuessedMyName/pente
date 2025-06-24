import Text.Read
import Pente
import Minmax

inputInteger :: Int -> Int -> IO Int
inputInteger low up = do
    str <- getLine
    case readMaybe str :: Maybe Int of
        Just number -> if (low <= number && number < up) then (return number) 
            else do putStrLn ("Please input a number in range " ++ show low ++ ", " ++ show up); inputInteger low up
        Nothing -> do putStrLn ("Please input a number in range " ++ show low ++ ", " ++ show up); inputInteger low up

play :: Pente -> Bool -> Int -> IO ()
play WinA _ _ = putStrLn "Player A wins!"
play WinB _ _ = putStrLn "Player B wins!"
play pente@(Pente player board pairsA pairsB sizeX sizeY) computer depth = do
    putStrLn (show pente)
    putStrLn "X:"
    x <- inputInteger 0 sizeX
    putStrLn "Y:"
    y <- inputInteger 0 sizeY
    if computer then do
        let nxt = nextPente pente (x+y*sizeY)
            comp = solve nxt depth
        play comp computer depth
    else
        play (nextPente pente (x+y*sizeY)) computer depth


main :: IO ()
main = do play (newBoard 8) True 2
    
