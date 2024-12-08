import Data.List (subsequences)

getInput :: FilePath -> IO [[Int]]
getInput filePath = do
    reports <- readFile filePath
    return (map (map read . words) (lines reports))

isSafe :: [Int] -> Bool
isSafe xs = case scores of
    xs -> all (\x -> 0 < x && x <= 3) xs || all (\x -> -3 <= x && x < 0) xs
  where scores = zipWith (-) xs (tail xs)

isSafeWithDamper :: [Int] -> Bool
isSafeWithDamper xs
    | isSafe xs = True
    | otherwise = any isSafe . filter (\x -> length x == newLength) $ subsequences xs
  where newLength = length xs - 1

main = do
    exampleReports <- getInput "example.txt"
    reports <- getInput "input.txt"

    -- part1
    putStrLn "\npart1"
    let example1 = length $ filter id (map isSafe exampleReports)
    print [example1, example1 - 2]
    let part1 = length $ filter id (map isSafe reports)
    print [part1, part1 - 252]
    
    -- part2
    putStrLn "\npart2"
    let example2 = length $ filter id (map isSafeWithDamper exampleReports)
    print [example2, example2 - 4]
    let part2 = length $ filter id (map isSafeWithDamper reports)
    print [part2, part2 - 324]