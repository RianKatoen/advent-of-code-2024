import Prelude hiding (traverse, Right, Left)
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

step :: [[Char]] -> Char -> [(Int, Int)] -> [(Int, Int)]
step flowers flower region
        | n             <- length flowers,
          possibleMoves <- concatMap (filter (inBounds n) . cross) region,
          actualMoves   <- filter (\(xx, yy) -> flowers !! xx !! yy == flower) possibleMoves,
          newMoves      <- filter (`notElem` region) actualMoves,
          newRegion     <- nub $ region ++ newMoves
        = if null newMoves then region else step flowers flower newRegion

region :: [[Char]] -> (Int, Int) -> [(Int, Int)]
region flowers (x, y) = step flowers (flowers !! x !! y) [(x, y)]

area :: [(Int, Int)] -> Int
area = length

perimeter :: [(Int, Int)] -> Int
perimeter region = area region * sum (map noNeighbours region)
    where noNeighbours (x, y) = 4 - length (filter (`elem` region) $ cross (x, y))

cornerCheck :: [(Int, Int)] -> (Int, Int) -> Int
cornerCheck region (x, y)
        = sum (map score1 cornerMoves)
    where cornerMoves       = [(Up, Right), (Right, Down), (Down, Left), (Left, Up)]
          selfMove          = (`elem` region) . move (x, y)
          diagMove (d1, d2) = (`elem` region) (move (move (x, y) d1) d2)
          score1 (d1, d2)   = if (not (selfMove d1) && not (selfMove d2)) || (selfMove d1 && selfMove d2 &&  not (diagMove (d1, d2))) then 1 else 0

findRegions :: [(Int, Int)] -> [[(Int, Int)]] -> [[Char]] -> [[(Int, Int)]]
findRegions unknownFlowers regions flowers
        | null unknownFlowers   = regions
        | otherwise             = findRegions newUnkownFlowers (nextRegion:regions) flowers
    where n                     = length flowers
          nextRegion            = region flowers $ head unknownFlowers
          newUnkownFlowers      = filter (`notElem` nextRegion) unknownFlowers

part1 :: [[Char]] -> Int
part1 flowers = sum . map perimeter $ findRegions (getIndices (length flowers)) [] flowers

part2 :: [[Char]] -> Int
part2 flowers = sum . map ((\r -> sum r * length r) . (\r -> map (cornerCheck r) r)) $ foundRegions
    where foundRegions = findRegions (getIndices (length flowers)) [] flowers

main = do
    exampleMap1 <- parseMap "example1.txt"
    exampleMap2 <- parseMap "example2.txt"
    exampleMap3 <- parseMap "example3.txt"
    inputMap <- parseMap "input.txt"

    putStrLn "\n--part1"
    let example11 = part1 exampleMap1
    print [example11, example11 - 140]
    let example12 = part1 exampleMap2
    print [example12, example12 - 772]
    let example13 = part1 exampleMap3
    print [example13, example13 - 1930]
    let answerPart1 = part1 inputMap
    print [answerPart1, answerPart1 - 1371306]

    putStrLn "\n--part2"
    let example21 = part2 exampleMap1
    print [example21, example21 - 80]
    let example23 = part2 exampleMap3
    print [example23, example23 - 1206]
    let answerPart2 = part2 inputMap
    print [answerPart2, answerPart2 - 1371306]
