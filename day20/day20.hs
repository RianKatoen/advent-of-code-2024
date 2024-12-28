import qualified Data.Map.Strict as Map

import Data.List (elemIndex)
import Data.Maybe (mapMaybe, isNothing, fromJust, catMaybes)

data Direction = East | West | North | South deriving (Eq, Ord, Show)
allDirections = [East, West, North, South]


parseFile :: FilePath -> IO ([String], (Int, Int))
parseFile filePath = do
        contents <- readFile filePath
        let maze = lines contents
        let loc = head . mapMaybe checkLine $ zip [0..] maze
        return (maze, loc)
    where whereIsStart = elemIndex 'S'
          checkLine (x, y)  = if isNothing (whereIsStart y) then Nothing else Just (x, fromJust $ whereIsStart y)

move :: Direction -> (Int, Int) -> (Int, Int)
move d (x, y) = case d of
    West   -> (x, y - 1)
    East   -> (x, y + 1)
    North  -> (x - 1, y)
    South  -> (x + 1, y)

inBounds :: [[Char]] -> (Int, Int) -> Bool
inBounds maze (x, y) = maze !! x !! y /= '#'

traverseMaze :: [[Char]] -> [(Int, Int)] -> [(Int, Int)]
traverseMaze maze [loc] = traverseMaze maze $ filter (inBounds maze) (map (`move` loc) allDirections) ++ [loc]
traverseMaze maze (loc : prev : rest)
        | maze !! x !! y == 'E' = reverse newHistory
        | otherwise             = traverseMaze maze newHistory

    where (x, y)     = head . filter (inBounds maze) $ filter (/= prev) $ map (`move` loc) allDirections
          newHistory = (x, y) : (loc:prev:rest)

cheat :: [[Char]] -> [(Int, Int)] -> [[Int]]
cheat maze solution = amountSaved
    where solutions         = Map.fromList $ zip solution [0..]
          possibleCheats    = map (\(i, j) -> catMaybes [Map.lookup (i + 2, j) solutions, Map.lookup (i - 2, j) solutions, Map.lookup (i, j + 2) solutions, Map.lookup (i, j - 2) solutions]) solution
          amountSaved       = zipWith (\i js -> map (\j -> j - 2 - i) $ filter (\ j -> j > i + 2) js) [0..] possibleCheats

part1 :: Int -> [[Char]] -> (Int, Int) -> Int
part1 threshold maze loc = length $ concatMap (filter (>=threshold)) (cheat maze $ traverseMaze maze [loc])

main = do
    (exampleMaze, exampleLoc) <- parseFile "example.txt"
    (maze, loc) <- parseFile "input.txt"

    putStrLn "\n--part 1"
    let example1 =  part1 0 exampleMaze exampleLoc
    print [example1, example1 - 44]

    let answerPart1 =  part1 100 maze loc
    print [answerPart1, answerPart1 - 1389]