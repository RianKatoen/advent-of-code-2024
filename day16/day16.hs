import qualified Data.Bifunctor
import qualified Data.Map.Strict as Map

import Data.List (elemIndex)
import Data.Maybe (mapMaybe, isNothing, fromJust, isJust, fromMaybe)

data Direction = East | West | North | South deriving (Eq, Ord, Show)
data Step = Turn | Forward deriving (Eq, Ord, Show)

parseFile filePath = do
        contents <- readFile filePath
        let maze = lines contents
        let loc = head . mapMaybe checkLine $ zip [0..] maze
        return (maze, (East, loc))
    where whereIsStart = elemIndex 'S'
          checkLine (x, y)  = if isNothing (whereIsStart y) then Nothing else Just (x, fromJust $ whereIsStart y)

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

cross :: (Int, Int) -> [(Int, Int)]
cross (x, y) = map (\d -> move (d, (x, y))) [East, West, North, South]

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

traverseMaze :: [[Char]] -> (Map.Map (Int, Int) Int, Map.Map (Direction, (Int, Int)) Int) -> (Map.Map (Int, Int) Int, Map.Map (Direction, (Int, Int)) Int)
traverseMaze maze (visitedLocs, newLocs)
        | null differenceLocs      = (newVisitedLocs, differenceLocs)
        | otherwise                = traverseMaze maze (newVisitedLocs, differenceLocs)
    where newLocsList       = Map.toList newLocs
          rotatedLocs1      = map (Data.Bifunctor.first fromJust) . filter (isJust . fst) $ map (\(loc, val) -> (step maze $ rotate loc, val + 1001)) newLocsList
          rotatedLocs2      = map (Data.Bifunctor.first fromJust) . filter (isJust . fst) $ map (\(loc, val) -> (step maze $ ccrotate loc, val + 1001)) newLocsList
          movedLocs         = map (Data.Bifunctor.first fromJust) . filter (isJust . fst) $ map (\(loc, val) -> (step maze loc, val + 1)) newLocsList
          afterMoveLocs     = rotatedLocs1 ++ rotatedLocs2 ++ movedLocs
          newVisitedLocs    = Map.unionWith min visitedLocs $ Map.fromListWith min $ map (\((_, loc), val) -> (loc, val)) afterMoveLocs
          lookupLoc (x, y)  = fromMaybe 99999999 $ Map.lookup (x, y) newVisitedLocs
          differenceLocs    = Map.fromList $ filter (\((_, loc), val) -> val <= lookupLoc loc) afterMoveLocs

printMaze :: [[Char]] -> [(Direction, (Int, Int))] -> [[Char]]
printMaze maze locs =
     zipWith (\r w -> zipWith (\c cc -> if Map.member (r, c) locMap then printReindeer (fromJust $ Map.lookup (r,c) locMap) else cc) [0..] w) [0..] maze
     where locMap = Map.fromList $ map (\(x, y) -> (y, x)) locs

part1 :: [[Char]] -> (Direction, (Int, Int)) -> Int
part1 maze loc = minimum $ map snd $ filter (\((x, y), _) -> maze !! x !! y == 'E') $ Map.toList $ fst $ traverseMaze maze (Map.empty, Map.fromList [(loc, 0)])

reverseTraverse :: Map.Map (Int, Int) Int -> (Int, Int) ->  [(Int, Int)]
reverseTraverse steps loc 
        | loc `Map.notMember` steps  = []
        | otherwise                  = loc : concatMap (reverseTraverse steps) crossMoves
    where (l, v)    = (loc, fromJust $ Map.lookup loc steps)
          crossMoves = map fst $ filter (\(_, y) -> y < v) $ map (\x -> (x, fromMaybe 99999999 (Map.lookup x steps))) $ cross loc

-- part2 :: [[Char]] -> (Direction, (Int, Int)) -> Int
part2 maze loc = reverseTraverse (fst $ traverseMaze maze (Map.empty, Map.fromList [(loc, 0)])) (1,13)

main = do
    (mazeExample1, locExample1) <- parseFile "example1.txt"
    (mazeExample2, locExample2) <- parseFile "example2.txt"
    (mazeInput, locInput) <- parseFile "input.txt"

    putStrLn "\npart 1"
    print $ part1 mazeExample1 locExample1
    print $ part1 mazeExample2 locExample2
    print $ part1 mazeInput locInput

    putStrLn "\npart 2"
    -- mapM_ print $ Map.toList $ fst $ traverseMaze mazeExample1 (Map.empty, Map.fromList [(locExample1, 0)])
    mapM_ print $ printMaze mazeExample1 $ map (West,) $ part2 mazeExample1 locExample1
