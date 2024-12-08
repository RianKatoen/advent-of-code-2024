import Data.List (groupBy, nub, sortBy)
import Debug.Trace (traceShowId)
import Control.Monad.RWS (MonadState(put))

data Antenna = Antenna { freq :: Char, loc :: (Int, Int)}
    deriving(Show, Eq)

data Antennas = Antennas { frequency :: Char, locations :: [(Int, Int)]}
    deriving(Show)

-- Still not familiar with sharing code.
split :: Eq a => a -> [a] -> [[a]]
split c s = case rest of
        []     -> [chunk]
        _:rest -> chunk : split c rest
    where (chunk, rest) = break (==c) s

parseLine :: Int -> String -> [Antenna]
parseLine row = filter (\(Antenna f l) -> f /= '.' && f/= '#') . zipWith (\ col c -> Antenna c (row, col)) [0..]

parseFile :: FilePath -> IO (Int, [Antenna])
parseFile filePath = do
    stuff <- readFile filePath
    let linez = lines stuff
    return (length linez, concat $ filter (not . null) $ zipWith parseLine [0..] linez)

-- This seems so stupid.
orderAntenna :: Ord a => a -> a -> Ordering
orderAntenna a b
    | a > b       = GT
    | b < a       = LT
    | otherwise   = EQ

groupAntennas :: [Antenna] -> [[Antenna]]
groupAntennas = groupBy (\(Antenna a _) (Antenna b _) -> a == b) . sortBy (\(Antenna a _) (Antenna b _) -> orderAntenna a b)

isInBounds :: Int -> (Int, Int) -> Bool
isInBounds u (x, y) = x >= 0 && y >= 0 && x < u && y < u

antiNodes :: Bool -> Int -> [(Int, Int)] -> [(Int, Int)]
antiNodes _ _ [a]                   = []
antiNodes rep u [(xa, ya), (xb, yb)] |
        (vx, vy) <- (xb - xa , yb - ya),
        indices  <- if rep then [0..] else [1],
        as       <- takeWhile (isInBounds u) $ map (\i -> (xa - i * vx, ya - i * vy)) indices,
        bs       <- takeWhile (isInBounds u) $ map (\i -> (xb + i * vx, yb + i * vy)) indices
    = as ++ bs

antiNodes rep u (a:b:rest) = antiNodes rep u [a, b] ++ antiNodes rep u (a:rest) ++ antiNodes rep u (b:rest)

allAntiNodes :: Bool -> Int -> [Antenna] -> [[(Int, Int)]]
allAntiNodes rep u |
        groupedAntennas <- groupAntennas
    = map (nub . antiNodes rep u . map (\(Antenna f loc) -> loc)) . groupedAntennas

main = do
    putStr "\nexample 1: "
    (boundExample1, example1) <- parseFile "example1.txt"
    mapM_ print $ allAntiNodes False boundExample1 example1

    putStr "example 2: "
    (boundExample2, example2) <- parseFile "example2.txt"
    mapM_ print $ allAntiNodes False boundExample2 example2

    putStr "example 3: "
    (boundExample3, example3) <- parseFile "example3.txt"
    print $ length . nub . concat $ allAntiNodes False boundExample3 example3

    putStrLn "\npart 1"
    (boundInput, input) <- parseFile "input.txt"
    let part1 = length . nub . concat $ allAntiNodes False boundInput input
    print [part1, part1 - 293]

    putStr "\nexample 4: "
    (boundExample4, example4) <- parseFile "example4.txt"
    print $ length . nub . concat $ allAntiNodes True boundExample4 example4

    putStr "example 3: "
    -- mapM_ print $ groupAntennas example3
    print $ length . nub . concat $ allAntiNodes True boundExample3 example3

    putStrLn "\npart 2"
    -- mapM_ print $ groupAntennas example3
    let part2 = length . nub . concat $ allAntiNodes True boundInput input
    print [part2, part2 - 934]