import qualified Data.Bifunctor
import qualified Data.Map.Strict as Map

import Data.List (elemIndex, nub, sort)
import Data.Maybe (mapMaybe, isNothing, fromJust, isJust, fromMaybe)
import Debug.Trace (traceShowId)
import qualified GHC.Exts.Heap as Map

data Direction = East | West | North | South deriving (Eq, Ord, Show)
data Step = Turn | Forward deriving (Eq, Ord, Show)

parseFile filePath = do
        contents <- readFile filePath
        let maze = lines contents
        let loc = head . mapMaybe checkLine $ zip [0..] maze
        return (maze, (East, loc))
    where whereIsStart = elemIndex 'S'
          checkLine (x, y)  = if isNothing (whereIsStart y) then Nothing else Just (x, fromJust $ whereIsStart y)

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

inBounds :: Int -> (Int, Int) -> Bool
inBounds n (r, c) = r > 0 && r < n && c > 0 && c < n

(<++>) (a, b) (x, y) = (a + x, b + y)
(<**>) (a, b) z = (a * z, b * z)

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
          rotatedLocs1      = map (\(loc, val) -> (rotate loc, val + 1000)) newLocsList
          rotatedLocs2      = map (\(loc, val) -> (ccrotate loc, val + 1000)) newLocsList
          movedLocs         = map (Data.Bifunctor.first fromJust) . filter (isJust . fst) $ map (\(loc, val) -> (step maze loc, val + 1)) newLocsList
          afterMoveLocs     = rotatedLocs1 ++ rotatedLocs2 ++ movedLocs
          newVisitedLocs    = Map.unionWith min visitedLocs $ Map.fromListWith min afterMoveLocs
          lookupLoc loc     = fromMaybe 99999999999 $ Map.lookup loc newVisitedLocs
          differenceLocs    = Map.fromList $ filter (\(loc, val) -> val <= lookupLoc loc) afterMoveLocs

solveMaze :: [[Char]] -> (Direction, (Int, Int)) -> Map.Map (Direction, (Int, Int)) Int
solveMaze maze loc = fst $ traverseMaze maze (Map.fromList [(loc, 0)], Map.fromList [(loc, 0)])

printMaze :: [[Char]] -> [(Direction, (Int, Int))] -> [[Char]]
printMaze maze locs =
     zipWith (\r w -> zipWith (\c cc -> if Map.member (r, c) locMap then printReindeer (fromJust $ Map.lookup (r,c) locMap) else cc) [0..] w) [0..] maze
     where locMap = Map.fromList $ map (\(x, y) -> (y, x)) locs

part1 :: [[Char]] -> (Direction, (Int, Int)) -> Int
part1 maze loc = minimum $ map snd $ filter (\((x, y), _) -> maze !! x !! y == 'E') $ map (\((_, loc), v) -> (loc, v)) $ Map.toList $ solveMaze maze loc

reverseTraverse :: Map.Map (Direction, (Int, Int)) Int -> (Direction, (Int, Int)) -> Int -> [(Int, Int)]
reverseTraverse solvedMap (dir, loc) value
        | null newValues    = []
        | otherwise         = loc : concatMap (\((d, l), v) -> reverseTraverse solvedMap (d, l) v) newValues
    where locs      = Map.filterWithKey (\(d, l) v -> l == loc && ((d == dir && v <= value) || (d == rot dir || d == ccrot dir) && v == value - 1000)) solvedMap
          newLocs   = map (\((d, loc), v) -> ((d, reverseMove (d, loc)), v)) $ Map.toList locs
          newValues = map (\(x, _, w) -> (x, w)) $ filter (\(_, v, w) -> v > w) $ map (\(l, v) -> (l, v, fromMaybe 99999999999 (Map.lookup l solvedMap))) newLocs

part2 :: [[Char]] -> (Direction, (Int, Int)) -> Int -> Int
part2 maze loc score = 1 + length (nub $ reverseTraverse (solveMaze maze loc) (North, (1, length maze - 2)) score)

main = do
    (mazeExample1, locExample1) <- parseFile "example1.txt"
    (mazeExample2, locExample2) <- parseFile "example2.txt"
    (mazeInput, locInput) <- parseFile "input.txt"

    putStrLn "\npart 1"
    let example11 = part1 mazeExample1 locExample1
    print [example11, example11 - 7036]
    let example12 = part1 mazeExample2 locExample2
    print [example12, example12 - 11048]
    let answerPart1 = part1 mazeInput locInput
    print [answerPart1, answerPart1 - 123540]

    putStrLn "\npart 2"
    let example21 = part2 mazeExample1 locExample1 example11
    print [example21, example21 - 45]
    let example22 = part2 mazeExample2 locExample2 example12
    print [example22, example22 - 64]
    let answerPart2 = part2 mazeInput locInput answerPart1
    print [answerPart2, answerPart2 - 665]