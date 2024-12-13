import Prelude hiding (traverse, Right, Left)
import qualified Data.Set as Sets
import Debug.Trace (traceShowId, trace, traceShowM)
import Data.List (nub)

parseMap :: FilePath -> IO [[Char]]
parseMap filePath = do
    contents <- readFile filePath
    return $ lines contents

data Direction = Left | Right | Up | Down deriving (Eq, Ord, Show)

getIndices :: Int -> [(Int, Int)]
getIndices n = take (n*n) $ map (\i -> (i `div` n, i `rem` n)) [0..]

inBounds :: Int -> (Int, Int) -> Bool
inBounds n (x, y) = x >= 0 && x < n && y >= 0 && y < n

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) d = case d of
    Left    -> (x, y - 1)
    Right   -> (x, y + 1)
    Up      -> (x - 1, y)
    Down    -> (x + 1, y)

cross :: (Int, Int) -> [(Int, Int)]
cross (x, y) = map (move (x, y)) [Up, Down, Left, Right]

filterFlower :: Char -> [[Char]] -> [[Char]]
filterFlower flower = map (map removeFlower)
    where removeFlower f = if f == flower then flower else '.'

step :: [[Char]] -> Char -> Sets.Set (Int, Int) -> Sets.Set (Int, Int)
step flowers flower region
        | n             <- length flowers,
          possibleMoves <- Sets.fromList $ concatMap (filter (inBounds n) . cross) region,
          actualMoves   <- Sets.filter (\(xx, yy) -> flowers !! xx !! yy == flower) possibleMoves,
          newMoves      <- Sets.filter (`notElem` region) actualMoves,
          newRegion     <- Sets.union region newMoves
        = if null newMoves then region else step flowers flower newRegion

region :: [[Char]] -> (Int, Int) -> Sets.Set (Int, Int)
region flowers (x, y) = step flowers (flowers !! x !! y) (Sets.fromList [(x, y)])

area :: Sets.Set (Int, Int) -> Int
area = Sets.size

perimeter :: Sets.Set (Int, Int) -> Int
perimeter region = Sets.size region * sum (map noNeighbours $ Sets.toList region)
    where noNeighbours (x, y) = 4 - length (filter (`elem` region) $ cross (x, y))

findRegions :: [(Int, Int)] -> [Sets.Set (Int, Int)] -> [[Char]] -> [Sets.Set (Int, Int)]
findRegions unknownFlowers regions flowers
        | null unknownFlowers   = regions
        | otherwise             = findRegions newUnkownFlowers (nextRegion:regions) flowers
    where n                     = length flowers
          nextRegion            = region flowers $ head unknownFlowers
          newUnkownFlowers      = filter (`notElem` nextRegion) unknownFlowers


part1 :: [[Char]] -> Int
part1 flowers = sum . map perimeter $ findRegions (getIndices (length flowers)) [] flowers

main = do
    exampleMap1 <- parseMap "example1.txt"
    exampleMap2 <- parseMap "example2.txt"
    exampleMap3 <- parseMap "example3.txt"
    inputMap <- parseMap "input.txt"

    putStrLn "\n--part1"
    let example1 = part1 exampleMap1
    print [example1, example1 - 140]
    let example2 = part1 exampleMap2
    print [example2, example2 - 772]
    let example3 = part1 exampleMap3
    print [example3, example3 - 1930]
    let answerPart1 = part1 inputMap
    print [answerPart1, answerPart1 - 1371306]

    -- print $ perimeter $ region inputMap (0, 0)
