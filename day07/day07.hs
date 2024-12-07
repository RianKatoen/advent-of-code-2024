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

addddddd :: Integer -> Integer -> Integer
addddddd = (+)

productt :: Integer -> Integer -> Integer
productt = (*)

concattt :: (Show a1, Show a2) => a1 -> a2 -> Integer
concattt a b = read (show a ++ show b) :: Integer

-- The fun part :)
getOperators :: (Eq a, Num a) => a -> [[Integer -> Integer -> Integer]]
getOperators 1 = [[(+)], [(*)]]
getOperators n | addPlus    <- map (addddddd :) nextLevel,
                 addProduct <- map (productt :) nextLevel
             = addPlus ++ addProduct
             where nextLevel = getOperators (n - 1)

apply :: [t1] -> [t1 -> t1 -> t1] -> t1
apply [a, b]     [operator]          = operator a b
apply (a:b:rest) (o:operators)     | stuff <- o a b
                = apply (stuff : rest) operators

solve :: Num b => (Int -> [[Integer -> Integer -> Integer]]) -> Equation -> b
solve getOperatorss (Equation result values)
        | allPossibleOperators <- getOperatorss (length values - 1),
          allResults           <- map (apply values) allPossibleOperators,
          allProperResults     <- filter (== result) allResults
    = fromIntegral (length allProperResults)

printOp :: (Integer -> Integer -> Integer) -> Char
printOp op = case outcome of
        2   -> '+'
        1   -> '*'
        11  -> '|'
    where outcome = op 1 1 


part1 :: [Equation] -> Integer
part1 eqs = sum $ map (\eq -> if solve getOperators eq > 0 then result eq else 0) eqs

getOperatorz :: (Eq a, Num a) => a -> [[Integer -> Integer -> Integer]]
getOperatorz 1 = [[(+)], [(*)], [concattt]]
getOperatorz n | addPlus    <- map (addddddd :) nextLevel,
                 addProduct <- map (productt :) nextLevel,
                 addConcat  <- map (concattt :) nextLevel
             = addPlus ++ addProduct ++ addConcat
             where nextLevel = getOperatorz (n - 1)

part2 :: [Equation] -> Integer
part2 eqs = sum $ map (\eq -> if solve getOperatorz eq > 0 then result eq else 0) eqs

-- main yo.
main = do
    exampleEquations <- parseFile "example.txt"
    inputEquations <- parseFile "input.txt"

    putStrLn "\n--part1"
    let example1 = part1 exampleEquations
    print [example1, example1 - 3749]
    let answerPart1 = part1 inputEquations
    print [answerPart1, answerPart1 - 1620690235709]

    
    putStrLn "\n--part2"
    let example2 = part2 exampleEquations
    print [example2, example2 - 11387]

    let answerPart2 = part2 inputEquations
    print [answerPart2, answerPart2 - 145397611075341]