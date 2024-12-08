-- Most important data structure.
data Equation = Equation { result :: Integer, values :: [Integer] }
    deriving (Show)

-- Still not familiar with sharing code.
split :: Eq a => a -> [a] -> [[a]]
split c s = case rest of
        []     -> [chunk]
        _:rest -> chunk : split c rest
    where (chunk, rest) = break (==c) s

-- General parsing stuff.
parseLine :: String -> Equation
parseLine line | [result, valuesString] <- split ':' line,
                 valueStrings           <- words valuesString
            = Equation (read @Integer result) (map (read @Integer) valueStrings)

parseFile :: FilePath -> IO [Equation]
parseFile filePath = do
    stuff <- readFile filePath
    let equationStrings = lines stuff
    return $ map parseLine equationStrings

--concattt :: (Inte a1, Show a2) => a1 -> a2 -> Integer
concattt :: Integral a => a -> a -> a
concattt a b = a * 10^(1 + log10 b) + b
    where log10 x = floor (logBase 10 (fromIntegral x))

-- The fun part :)
getOperators :: (Eq a, Num a) => [Integer -> Integer -> Integer] -> a -> [[Integer -> Integer -> Integer]]
getOperators base 1 = map (: []) base
getOperators base n
        = concatMap joinOps base
    where   nextLevel = getOperators base (n - 1)
            joinOps op = map (op :) nextLevel

apply :: [t1] -> [t1 -> t1 -> t1] -> t1
apply [a, b]     [operator]          = operator a b
apply (a:b:rest) (o:operators)     | stuff <- o a b
                = apply (stuff : rest) operators

solve :: (Int -> [[Integer -> Integer -> Integer]]) -> Equation -> Bool
solve operators (Equation result values)
        | allPossibleOperators <- operators (length values - 1),
          allResults           <- map (apply values) allPossibleOperators
    = result `elem` allResults

printOp :: (Integer -> Integer -> Integer) -> Char
printOp op = case outcome of
        2   -> '+'
        1   -> '*'
        11  -> '|'
    where outcome = op 1 1


solution :: [Integer -> Integer -> Integer] -> [Equation] -> Integer
solution ops eqs = sum $ map (\eq -> if solve (getOperators ops) eq then result eq else 0) eqs

-- main yo.
main = do
    exampleEquations <- parseFile "example.txt"
    inputEquations <- parseFile "input.txt"

    putStrLn "\n--part1"
    let example1 = solution [(+), (*)] exampleEquations
    print [example1, example1 - 3749]
    let answerPart1 = solution [(+), (*)] inputEquations
    print [answerPart1, answerPart1 - 1620690235709]

    putStrLn "\n--part2"
    let example2 = solution [(+), (*), concattt] exampleEquations
    print [example2, example2 - 11387]

    let answerPart2 = solution [(+), (*), concattt] inputEquations
    print [answerPart2, answerPart2 - 145397611075341]