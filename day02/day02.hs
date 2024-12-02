import System.IO (readFile)
import Data.List (subsequences)

readFileToColumns :: FilePath -> IO [[Int]]
readFileToColumns filePath = do
    reports <- readFile filePath
    return (map (map read . words) (lines reports))

--calculateSafetyScores :: [Int] -> [Int]
calculateSafetyScores x = zipWith (-) x (tail x)

isSafe :: [Int] -> Bool
isSafe xs
  | all (\x -> 0 < x && x <= 3) (calculateSafetyScores xs) = True
  | all (\x -> -3 <= x && x < 0) (calculateSafetyScores xs) = True
  | otherwise = False

isSafeWithDamper :: [Int] -> Bool
isSafeWithDamper xs
  | isSafe xs = True
  | otherwise = any isSafe (filter (\x -> length x == length xs - 1) (subsequences xs))

-- Main function for testing
main :: IO ()
main = do
    exampleReports <- readFileToColumns "example.txt"
    reports <- readFileToColumns "input.txt"

    -- part1
    putStrLn "\npart1"
    print $ length $ filter id (map isSafe exampleReports)
    print $ length $ filter id (map isSafe reports)

    -- part2
    putStrLn "\npart2"
    print $ length $ filter id (map isSafeWithDamper exampleReports)
    print $ length $ filter id (map isSafeWithDamper reports)
