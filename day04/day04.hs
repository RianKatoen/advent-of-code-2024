data XDirection = GoUp | GoDown | Flat deriving (Show, Eq, Enum)
data YDirection = GoLeft | GoRight | Center deriving (Show, Eq, Enum)

allDirections :: [(XDirection, YDirection)]
allDirections = filter (/= (Flat, Center)) $ zip (concatMap (replicate 3) [GoUp .. Flat]) $ concat $ replicate 3 [GoLeft .. Center]

getXOffset :: XDirection -> Int
getXOffset xDirection = case xDirection of
    GoUp -> -1
    GoDown -> 1
    _ -> 0

getYOffset :: YDirection -> Int
getYOffset yDirection = case yDirection of
    GoLeft -> -1
    GoRight -> 1
    _ -> 0

getOffset :: (XDirection, YDirection) -> (Int, Int)
getOffset (xDirection, yDirection) = (getXOffset xDirection, getYOffset yDirection)

checkBounds :: (Int, Int) -> Int -> Bool
checkBounds (x, y) boundary
    | x < 0          = False
    | y < 0          = False
    | x >= boundary  = False
    | y >= boundary  = False
    | otherwise      = True

getIndices :: Int -> [(Int, Int)]
getIndices n = take (n*n) $ map (\i -> (i `div` n, i `rem` n)) [0..]

getWord :: Int -> [String] ->  (Int, Int) -> (XDirection, YDirection) -> String
getWord n puzzle (x, y) (xDir, yDir)
    | not $ checkBounds (x, y) boundary                         = []
    | checkBounds ((n - 1) * dx + x, (n - 1) * dy + y) boundary = getLetters
    | otherwise                                                 = []
    where boundary      = length puzzle
          (dx, dy)      = getOffset (xDir, yDir)
          getLetters    = take n $ map (\i -> puzzle !! (x + i * dx) !! (y + i * dy)) [0..]

getWords :: Int -> [String] -> (Int, Int) -> [String]
getWords n puzzle (x, y) = filter (/="") $ map getWordPuzzle allDirections
    where getWordPuzzle = getWord n puzzle (x, y)

countWord :: String -> [String] -> Int
countWord word puzzle = sum $ map (length . filter (==word) . getWordPuzzle) $ getIndices $ length puzzle
    where getWordPuzzle = getWords (length word) puzzle

getCross :: [String] -> (Int, Int) -> [String]
getCross puzzle (x, y) = [getWordPuzzle (x - 1, y - 1) (GoDown, GoRight), getWordPuzzle (x + 1, y - 1) (GoUp, GoRight)]
    where getWordPuzzle = getWord 3 puzzle

checkCross :: String -> [String] -> Bool
checkCross _    ["", _]  = False
checkCross _    [_, ""]  = False
checkCross word [a, b]   = (a == word || a == reverse word) && (a == b || a == reverse b)

countCrosses :: String -> [String] -> [[String]]
countCrosses word puzzle = filter (checkCross word) $ map (getCross puzzle) $ getIndices $ length puzzle

main = do
    inputFile <- readFile "input.txt"
    let input = lines inputFile

    putStrLn "\n--part1"
    example1InputFile <- readFile "example1.txt"
    let example1Input = lines example1InputFile
    let example1 = countWord "XMAS" example1Input
    print [example1, example1 - 18]

    let answerPart1 = countWord "XMAS" input
    print [answerPart1, answerPart1 - 2569]


    putStrLn "\n--part2"

    example2InputFile <- readFile "example2.txt"
    let example2Input = lines example2InputFile
    let example2 = length $ countCrosses "MAS" example2Input
    print [example2, example2 - 9]

    let answerPart2 = length $ countCrosses "MAS" input
    print [answerPart2, answerPart2 - 1998]