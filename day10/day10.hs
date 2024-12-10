import Prelude hiding (Right, Left)
import Data.Char (digitToInt)
import Data.List (nub)

parseMap :: FilePath -> IO [[Int]]
parseMap filePath = do
    contents <- readFile filePath
    return $ map (map digitToInt) $ lines contents

getIndices :: Int -> [(Int, Int)]
getIndices n = take (n*n) $ map (\i -> (i `div` n, i `rem` n)) [0..]

data Direction = Left | Right | Up | Down deriving (Eq, Ord, Show)

inBounds :: Int -> (Int, Int) -> Bool
inBounds n (x, y) = x >= 0 && x < n && y >= 0 && y < n

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) d = case d of
    Left    -> (x, y - 1)
    Right   -> (x, y + 1)
    Up      -> (x - 1, y)
    Down    -> (x + 1, y)

steps :: [[Int]] -> Int -> (Int, Int) -> [(Int, Int)]
steps trail n (x, y) = filter (\(xx, yy) -> (trail !! xx !! yy) - current_height == 1) $ filter (inBounds n) next_locations
    where next_locations = map (move (x, y)) [Left, Right, Up, Down]
          current_height = trail !! x !! y


walk :: [[Int]] -> Int -> (Int, Int) -> [(Int, Int)]
walk trail n (x, y)
        | trail !! x !! y == 9  = [(x, y)]
        | otherwise             = nub $ concatMap (walk trail n) next_steps
    where next_steps = steps trail n (x, y)

rate :: [[Int]] -> Int -> (Int, Int) -> Int
rate trail n (x, y)
        | trail !! x !! y == 9  = 1
        | otherwise             = sum $ map (rate trail n) next_steps
    where next_steps = steps trail n (x, y)

scan :: (Eq a, Num a) => ([[a]] -> Int -> (Int, Int) -> b) -> [[a]] -> [b]
scan f trail = map (f trail n) (filter (\(x, y) -> trail !! x !! y == 0) $ getIndices n)
    where n = length trail

part1 :: [[Int]] -> Int
part1 = length . concat . scan walk

part2 :: [[Int]] -> Int
part2 = sum . scan rate

main = do
    exampleMap <- parseMap "example.txt"
    inputMap <- parseMap "input.txt"

    putStrLn "\npart 1"
    let example1 = part1 exampleMap
    print [example1, example1 - 36]
    let answerPart1 = part1 inputMap
    print [answerPart1, answerPart1 - 512]

    putStrLn "\npart 2"
    let example2 = part2 exampleMap
    print [example2, example2 - 81]
    let answerPart2 = part2 inputMap
    print [answerPart2, answerPart2 - 1045]
