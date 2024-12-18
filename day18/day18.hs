
import qualified Data.Bifunctor
import qualified Data.Map.Strict as Map

import Data.List (elemIndex, nub, sort)
import Data.Maybe (mapMaybe, isNothing, fromJust, isJust, fromMaybe)
import Debug.Trace (traceShowId, traceShow)

parseBytes filePath = do
        contents <- readFile filePath
        let bytes = map (readLine . break (==',')) $ lines contents
        return bytes
    where readLine (a, b) = (read @Int a, read @Int $ tail b)

emptyMap :: Int -> [[Char]]
emptyMap n = horizontalWall ++ replicate (n - 2) middlePart ++ horizontalWall
    where horizontalWall = [replicate n '#']
          middlePart     = '#' : replicate (n - 2) '.' ++ ['#']

replace :: [[Char]] -> [(Char, (Int, Int))] -> [[Char]]
replace byteMap [(char, (r, c))] = zipWith (\rr row -> if rr /= r then row else zipWith (\cc o -> if cc /= c then o else char) [0..] row) [0..] byteMap
replace byteMap ((char, (r, c)):rest) = replace (replace byteMap [(char, (r, c))]) rest

dropBytes :: [[Char]] -> [(Int, Int)] -> [[Char]]
dropBytes byteMap bytes = replace byteMap $ map (\(x, y) -> ('#', (y + 1, x + 1))) bytes

---------------------------day 16
data Direction = East | West | North | South deriving (Eq, Ord, Show)
data Step = Turn | Forward deriving (Eq, Ord, Show)

rot :: Direction -> Direction
rot d = case d of
    East    -> North
    North   -> West
    West    -> South
    South   -> East

ccrot :: Direction -> Direction
ccrot d = case d of
    East    -> South
    North   -> East
    West    -> North
    South   -> West

rotate :: (Direction, (Int, Int)) -> (Direction, (Int, Int))
rotate (d, loc) = case d of
    East    -> (North, loc)
    North   -> (West, loc)
    West    -> (South, loc)
    South   -> (East, loc)

ccrotate :: (Direction, (Int, Int)) -> (Direction, (Int, Int))
ccrotate (d, loc) = case d of
    East    -> (South, loc)
    North   -> (East, loc)
    West    -> (North, loc)
    South   -> (West, loc)

move :: (Direction, (Int, Int)) -> (Int, Int)
move (d, (x, y)) = case d of
    East    -> (x, y - 1)
    West    -> (x, y + 1)
    North   -> (x - 1, y)
    South   -> (x + 1, y)

reverseMove :: (Direction, (Int, Int)) -> (Int, Int)
reverseMove (d, (x, y)) = case d of
    East    -> (x, y + 1)
    West    -> (x, y - 1)
    North   -> (x + 1, y)
    South   -> (x - 1, y)

printReindeer :: Direction -> Char
printReindeer d = case d of
    East -> '<'
    West -> '>'
    North -> '^'
    South -> 'v'

step :: [[Char]] -> (Direction, (Int, Int)) -> Maybe (Direction, (Int, Int))
step maze (dir, loc)
        | isForwardLegit = Just (fd, (fx, fy))
        | otherwise      = Nothing
    where (fd, (fx, fy))   = (dir, move (dir, loc))
          isForwardLegit   = maze !! fx !! fy /= '#'

traverseMaze :: [[Char]] -> (Map.Map (Direction, (Int, Int)) Int, Map.Map (Direction, (Int, Int)) Int) -> (Map.Map (Direction, (Int, Int)) Int, Map.Map (Direction, (Int, Int)) Int)
traverseMaze maze (visitedLocs, newLocs)
        | null differenceLocs      = (newVisitedLocs, differenceLocs)
        | otherwise                = traverseMaze maze (newVisitedLocs, differenceLocs)
    where newLocsList       = Map.toList newLocs
          rotatedLocs1      = map (\(loc, val) -> (rotate loc, val + 1)) newLocsList
          rotatedLocs2      = map (\(loc, val) -> (ccrotate loc, val + 1)) newLocsList
          movedLocs         = map (Data.Bifunctor.first fromJust) . filter (isJust . fst) $ map (\(loc, val) -> (step maze loc, val + 1_000_000_000)) newLocsList
          afterMoveLocs     = rotatedLocs1 ++ rotatedLocs2 ++ movedLocs
          newVisitedLocs    = Map.unionWith min visitedLocs $ Map.fromListWith min afterMoveLocs
          lookupLoc loc     = fromMaybe 99999999999 $ Map.lookup loc newVisitedLocs
          differenceLocs    = Map.fromList $ filter (\(loc, val) -> val <= lookupLoc loc) afterMoveLocs

solveMaze :: [[Char]] -> (Int, Int) -> Map.Map (Direction, (Int, Int)) Int
solveMaze maze loc = fst $ traverseMaze maze (createDirections, createDirections)
    where createDirections = Map.fromList $ map (\d -> ((d, loc), 0)) [East, South]

part1 :: Int -> [(Int, Int)] -> Int
part1 n bytes = minimum $ map (\(_, score) -> score `div` 1_000_000_000) $ filter (\((_, (x, y)), _) -> x == (n + 1) && y == (n + 1)) $ Map.toList solution
    where solution = solveMaze (dropBytes (emptyMap (n + 3)) bytes) (1, 1)

-- part2 :: Int -> [(Int, Int)] -> Int -> Bool
part2 n bytes offset = (\i -> (i, bytes !! (i - 1))) (head (filter (null . end) [offset..]))
    where solution bts = solveMaze (dropBytes (emptyMap (n + 3)) bts) (1, 1)
          end k        = filter (\((_, (x, y)), _) -> x == (n + 1) && y == (n + 1)) $ Map.toList (solution $ take k bytes)

main = do
    exampleBytes <- parseBytes "example.txt"
    inputBytes <- parseBytes "input.txt"

    putStrLn "\n-- print example 1"
    mapM_ print $ dropBytes (emptyMap 9) (take 12 exampleBytes)

    putStrLn "\n-- part1"
    print $ part1 6 (take 12 exampleBytes)
    print $ part1 70 (take 1024 inputBytes)

    putStrLn "\n-- part2"
    print $ part2 6 exampleBytes 1
    print $ part2 70 inputBytes 3020