import Debug.Trace (traceShow, traceShowId)
import Data.List (findIndex, elemIndex, nub, sort)
import Data.Maybe (fromJust)
import Prelude hiding (Right, Left)
import Control.Arrow (ArrowLoop(loop))
import Control.Monad (guard)

data Direction = Left | Right | Up | Down
    deriving (Eq, Show)

data Pixel = Dot | Obj
    deriving (Eq, Show)

split :: Eq a => a -> [a] -> [[a]]
split c s = case rest of
        []     -> [chunk]
        _:rest -> chunk : split c rest
    where (chunk, rest) = break (==c) s

parseMap :: FilePath -> IO ((Int, Int), [[Pixel]])
parseMap filePath = do
    contents <- readFile filePath
    let elfMap = lines contents
    let (x, y) = head $ filter (\(x, y) -> y /= Nothing) $ zip [0..] (map (elemIndex '^') elfMap)
    return ((x, fromJust y), map (map (\x -> if x == '#' then Obj else Dot)) elfMap)

rotate :: Direction -> Direction
rotate d = case d of
    Left    -> Up
    Up      -> Right
    Right   -> Down
    Down    -> Left

move :: Direction -> (Int, Int) -> (Int, Int)
move d (x, y) = case d of
    Left    -> (x, y-1)
    Right   -> (x, y+1)
    Up      -> (x-1, y)
    Down    -> (x+1, y)

isOutOfBounds :: [[Pixel]] -> (Int, Int) -> Bool
isOutOfBounds _ (-1, _) = True
isOutOfBounds _ (_, -1) = True
isOutOfBounds elfMap (x, y)
    | x >= length elfMap = True
    | y >= length elfMap = True
    | otherwise          = False

step :: [[Pixel]] -> Direction -> (Int, Int) -> (Direction, (Int, Int), Bool)
step elfMap d (x, y)
        | isOutOfBounds elfMap (dx, dy) = (d, (x, y), True)
        | elfMap !! dx !! dy == Dot     = (d, (dx, dy), False)
        | otherwise                     = (rotate d, (x, y), False)
    where (dx, dy) = move d (x, y)

play :: [[Pixel]] -> [(Direction, (Int, Int))] -> Direction -> (Int, Int) -> [(Direction, (Int, Int))]
play elfMap steps d (x, y)
        | terminated                                = steps ++ [(d, (x, y))]
        | (d, (x, y)) `elem` steps                  = steps -- "Looping" condition.
        | otherwise                                 = play elfMap (steps ++ [(d, (x, y))]) dd (dx, dy)
    where (dd, (dx, dy), terminated) = step elfMap d (x, y)

getPath :: FilePath -> IO [(Direction, (Int, Int))]
getPath filePath = do
    (loc, elfMap) <- parseMap filePath
    return $ play elfMap [] Up loc

part1 :: FilePath -> IO Int
part1 filePath = do
    stuff <- getPath filePath
    let (directions, locs) = unzip stuff
    return $ length $ nub locs

placeObstacle :: (Int, Int) -> [[Pixel]] -> [[Pixel]]
placeObstacle (x, y) elfMap = do
    let (a, b:bs) = splitAt x elfMap
    let (aa, bb:bbs) = splitAt y b
    let henk = aa ++ [Obj] ++ bbs
    a ++ [henk] ++ bs

getAllPossibleObstacles :: [[Pixel]] -> [(Direction, (Int, Int))] -> [(Int, Int)]
getAllPossibleObstacles elfMap guardPath = filter (\(x, y) -> elfMap !! x !! y /= Obj) $ filter (not . isOutOfBounds elfMap) $ map (uncurry move) guardPath

superplay :: [[Pixel]] -> [(Direction, (Int, Int))] -> (Int, Int) -> Int -> [(Direction, (Int, Int))]
superplay elfMap guardPath loc i =
        play (placeObstacle loc elfMap) (take i guardPath) a b
    where (a, b) = guardPath !! i

part2 :: FilePath -> IO Int
part2 filePath = do
    (loc, elfMap) <- parseMap filePath
    let guardPath = play elfMap [] Up loc
    let allObstacles = getAllPossibleObstacles elfMap guardPath

    let jankMagic = filter (\(l, (x, y)) -> not (isOutOfBounds elfMap (x, y))) $ map ((\(l, (d, (x, y))) -> (l, move d (x, y))) . (\l -> (l, last $ superplay elfMap guardPath l 0))) allObstacles
    return $ length $ nub $ map fst jankMagic

main = do
    putStrLn "\n--part1"

    example1 <- part1 "example1.txt"
    print [example1, example1 - 41]

    answerPart1 <- part1 "input.txt"
    print [answerPart1, answerPart1 - 5444]

    putStrLn "\n--part2"
    example2 <- part2 "example1.txt"
    print [example2, example2 - 6]

    answerPart2 <- part2 "input.txt"
    print [answerPart2, answerPart2 - 1946]