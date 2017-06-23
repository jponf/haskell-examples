
import Control.Concurrent
import Data.Map
import System.Environment
import System.IO
import System.Random
import Text.Read

-- Main

main = do
    w <- requestInt "Width of the world? "
    h <- requestInt "Height of the world? "
    seed <- requestInt "World seed? "
    putStrLn $ "-- Grid Size: " ++ (show w) ++ "x" ++ (show h)
    putStrLn $ "-- World seed: " ++ (show seed)
    putStrLn ""
    runGameOfLife $ initialGrid (w, h) seed


runGameOfLife grid = do
    putStrLn ""
    printGrid grid
    threadDelay 500000
    runGameOfLife $ nextStep grid

printGrid grid = do
    let cells = getCells grid
    let (w, h) = getSize grid
    let possitions = [[(x, y) | x <- [1..w]] | y <- [1..h]]
    printGrid' cells possitions

printGrid' cells [] = do
    putStr ""
printGrid' cells (r:rs) = do
    printGridRow cells r
    printGrid' cells rs

printGridRow cells [] = do
    putStr ""
printGridRow cells [p] = do
    putStr $ cellRepr $ cells ! p
    putStrLn ""
printGridRow cells (p:ps) = do
    putStr $ (cellRepr (cells ! p)) ++ " "
    printGridRow cells ps

cellRepr :: Cell -> String
cellRepr c = case c of  Dead -> "D"
                        Alive -> "A"


-- Promp & User Input

{-
fusion :: (a -> Maybe b) -> (b -> Bool) -> (a -> Maybe b)
fusion f g = f >=> (\b -> if (g b) then Just b else Nothing)

prompt :: String -> IO String
prompt m = putStr (m ++ ": ") >> hFlush stdout >> getLine

rePrompt :: IO a -> (a -> Maybe b) -> (b -> Bool) -> IO b
rePrompt prompt find cond = do
    a <- prompt
    case find a of
        Nothing
-}

promp :: (Read a) => String -> IO (Maybe a)
promp s = putStr s >> hFlush stdout >> fmap readMaybe getLine
    
prompInt :: (Read a, Integral a) => String -> IO (Maybe a)
prompInt = promp

requestInt :: (Read a, Integral a) => String -> IO a
requestInt s = prompInt s >>= \v -> case v of 
    Just x -> return x
    Nothing -> putStrLn "Not a valid integer..." >> requestInt s


-- Life Game

type Size = (Int, Int)
type Pos = (Int, Int)
data Cell = Dead | Alive deriving (Enum, Show)
data Grid = Grid { getCells :: (Map (Int, Int) Cell), getSize :: Size } 
    deriving (Show)

instance Random Cell where
    random g = let (r, g') = randomR (0, 1) g 
        in (toEnum r, g')

    randomR (a, b) g = let (r, g') = randomR (fromEnum a, fromEnum b) g 
        in (toEnum r, g')


nextStep :: Grid -> Grid
nextStep grid =
    let (w, h) = getSize grid
        possitions = [(x, y) | x <- [1..w], y <- [1..h]]
        ncells = [nextCellStep grid p | p <- possitions]
    in
        Grid (fromList (zip possitions ncells)) (w, h)

nextCellStep grid pos =
    case cell of
        Dead 
            | nAlive == 3 -> Alive
            | otherwise -> Dead
        Alive
            | nAlive < 2 -> Dead
            | nAlive <= 3 -> Alive
            | nAlive > 3 -> Dead
    where
        cell = getCells grid ! pos
        nAlive = numAliveNeighbours grid pos


initialGrid :: Size -> Int -> Grid
initialGrid (w, h) seed =
    let possitions = [(i, j) | i <- [1..w], j <-[1..h]] 
        cells = fromList (zip possitions (randoms $ mkStdGen seed))
    in Grid cells (w, h)

neighbours :: Grid -> Pos -> [Pos]
neighbours grid (x, y) =
    [pos | i <- [-1..1], j <- [-1..1], 
              i /= 0 || j /= 0,
              let pos = (x+i, y+j),
              isInside grid pos]

numAliveNeighbours :: Grid -> Pos -> Int
numAliveNeighbours grid pos =
    Prelude.foldr (+) 0 [1 | npos <- neighbours grid pos, isAlive grid npos]

isInside :: Grid -> Pos -> Bool
isInside grid (x, y) = 
    let (w, h) = getSize grid
    in x > 0 && y > 0 && x <= w && y <= h

isAlive :: Grid -> Pos -> Bool
isAlive grid pos =
    let cells = getCells grid
    in case cells ! pos of
        Dead -> False
        Alive -> True
