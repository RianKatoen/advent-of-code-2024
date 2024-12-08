import Data.List (elemIndex, nub, sortBy, groupBy)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set, empty, member, insert, toList)
import Prelude hiding (Right, Left)
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShowId, traceShow)
import qualified Data.Bifunctor

data Direction = Left | Right | Up | Down deriving (Eq, Ord, Show)
data Pixel = Dot | Obj deriving (Eq, Ord, Show)

-- Own library when?
split :: Eq a => a -> [a] -> [[a]]
split c s = case rest of
        []     -> [chunk]
        _:rest -> chunk : split c rest
    where (chunk, rest) = break (==c) s

parseMap :: FilePath -> IO ((Int, Int), [[Pixel]])
parseMap filePath = do
    contents <- readFile filePath
    let linez = lines contents
    let (x, y) = head $ filter (\(x, y) -> isJust y) $ zip [0..] $ map (elemIndex '^') linez
    return ((x, fromJust y), map (map (\x -> if x == '#' then Obj else Dot)) linez)

-- Generic momevement stuff.
rotate :: Direction -> Direction
rotate d = case d of
    Left    -> Up
    Up      -> Right
    Right   -> Down
    Down    -> Left

move :: (Direction, (Int, Int)) -> (Int, Int)
move (d, (x, y)) = case d of
    Left    -> (x, y - 1)
    Right   -> (x, y + 1)
    Up      -> (x - 1, y)
    Down    -> (x + 1, y)

isOutOfBounds :: Int -> (Int, Int) -> Bool
isOutOfBounds _ (-1, _) = True
isOutOfBounds _ (_, -1) = True
isOutOfBounds boundary (x, y)
        | x >= boundary = True
        | y >= boundary = True
        | otherwise     = False

-- Movement logicz.
step :: [[Pixel]] -> Direction -> (Int, Int) -> (Direction, (Int, Int), Bool)
step elfMap d (x, y)
        | isOutOfBounds boundary (dx, dy) = (d,        (x, y),    True)
        | elfMap !! dx !! dy == Dot       = (d,        (dx, dy),  False)
        | otherwise                       = (rotate d, (x, y),    False)
    where (dx, dy) = move (d, (x, y))
          boundary = length elfMap

walk :: [[Pixel]] -> Int -> Map.Map (Direction, (Int, Int)) Int -> Direction -> (Int, Int) -> (Int, Map.Map (Direction, (Int, Int)) Int)
walk elfMap n steps d (x, y)
        | terminated                        = (n, Map.insert (d, (x, y)) n steps)
        | isJust wut                        = (-1, Map.insert (d, (x, y)) n steps) -- "Looping" condition.
        | otherwise                         = walk elfMap (n + 1) (Map.insert (d, (x, y)) n steps) dd (dx, dy)
    where (dd, (dx, dy), terminated) = step elfMap d (x, y)
          wut = Map.lookup (d, (x, y)) steps

part1 :: [[Pixel]] -> (Int, Int) -> Int
part1 elfMap loc = length . nub . map (\((_, (x, y)), _) -> (x, y)) $ y
    where x = snd $ walk elfMap 0 Map.empty Up loc
          y = Map.toList x

placeObstacle :: [[Pixel]] -> (Int, Int) -> [[Pixel]]
placeObstacle elfMap (x, y) = a ++ [between] ++ bs
    where (a, b:bs) = splitAt x elfMap
          (aa, bb:bbs) = splitAt y b
          between = aa ++ [Obj] ++ bbs

wuut :: (Int, (Int, Int)) -> (Int, (Int, Int)) -> Ordering
wuut (n, (a, b)) (m, (x, y))
    | (n, a, b) < (m, x, y)  = LT
    | (n, a, b) > (m, x, y)  = GT
    | otherwise              = EQ

getAllPossibleObstacles :: [[Pixel]] -> [((Direction, (Int, Int)), Int)] -> [(Int, (Int, Int))]
getAllPossibleObstacles elfMap guardPath
        | nextSteps         <- map (\((d, (x, y)), i) -> (i, move (d, (x, y)))) guardPath,
          boundary          <- length elfMap,
          inBoundsSteps     <- filter (not . isOutOfBounds boundary . snd) nextSteps
    = sortBy wuut $ filter (\(_, (x, y)) -> elfMap !! x !! y /= Obj) inBoundsSteps

compareObstacles :: (Int, (Int, Int)) -> (Int, (Int, Int)) -> Ordering
compareObstacles (n, (a, b)) (m, (x, y))
    | (a, b, n) < (x, y, m)  = LT
    | (a, b, n) > (x, y, m)  = GT
    | otherwise              = EQ

sameObstacle :: (Int, (Int, Int)) -> (Int, (Int, Int)) -> Bool
sameObstacle (_, a) (_, b) = a == b

sortGuardPath :: ((a, (b, c)), Int) -> ((a, (b, c)), Int) -> Ordering
sortGuardPath (_, a)  (_, b)
    | a < b     = LT
    | a > b     = GT
    | otherwise = EQ

part2 :: [[Pixel]] -> (Int, Int) -> Int
part2 elfMap loc = length . filter (== -1) $ newLastMoves
    where guardPath         = sortBy sortGuardPath $ Map.toList $ snd $ walk elfMap 0 Map.empty Up loc
          getStep ix        = fst $ guardPath !! ix
          getDirection ix   = fst $ getStep ix
          getLocation ix    = snd $ getStep ix
          allObstacles      = getAllPossibleObstacles elfMap guardPath
          uniqueObstacles   = map head . groupBy sameObstacle $ sortBy compareObstacles allObstacles
          newMaps           = map (Data.Bifunctor.second (placeObstacle elfMap)) uniqueObstacles
          getStuff i        = Map.fromList $ takeWhile (\(_, j) -> j < i) guardPath
          newLastMoves      = map (fst . (\(i, m) -> uncurry (walk m 0 (getStuff i)) (getStep i))) newMaps

        --   get i     = guardPath !! i

main = do
    (exampleLoc, exampleMap) <- parseMap "example1.txt"
    (loc, elfMap)            <- parseMap "input.txt"

    putStrLn "\n--part1"
    let example1 = part1 exampleMap exampleLoc
    print [example1, example1 - 41]

    let answerPart1 = part1 elfMap loc
    print [answerPart1, answerPart1 - 5444]

    putStrLn "\n--part2"
    let example2 = part2 exampleMap exampleLoc
    print [example2, example2 - 6]

    let answerPart2 = part2 elfMap loc
    print [answerPart2, answerPart2 - 1946]