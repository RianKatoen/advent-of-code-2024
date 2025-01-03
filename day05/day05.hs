import Data.List (sortBy)

split :: Char -> String -> [String]
split c s = case rest of
        []     -> [chunk]
        _:rest -> chunk : split c rest
    where (chunk, rest) = break (==c) s

parseFile :: FilePath -> IO ([(Int, Int)], [[Int]])
parseFile filePath = do
        contents <- readFile filePath
        let (orders, updates) = break (=="") $ lines contents
        return (map (order . split '|') orders, map update $ filter (/="") updates)
    where order     [x, y] = (read x :: Int, read y :: Int)
          update           = map (\x -> read x :: Int) . split ','

centerElem :: [a] -> a
centerElem [a] = a
centerElem as = centerElem (init $ tail as)

correctUpdate :: [(Int, Int)] -> [Int] -> Bool
correctUpdate _ [b] = True
correctUpdate as bs = notElem (nextb, b) as && correctUpdate as (tail bs)
    where b = head bs
          nextb = head $ tail bs

part1 :: [(Int, Int)] -> [[Int]] -> Int
part1 o u = sum $ map centerElem $ filter (correctUpdate o) u

part2 :: [(Int, Int)] -> [[Int]] -> Int
part2 o u = sum $ map (centerElem . sortBy pageOrdering) (filter (not . correctUpdate o) u)
    where pageOrdering a b
            | (a, b) `elem` o = LT
            | (b, a) `elem` o = GT
            | otherwise       = EQ

main = do
    (exampleOrders, exampleUpdates) <- parseFile "example.txt"
    (inputOrders, inputUpdates) <- parseFile "input.txt"

    putStrLn "\n--part1"
    let example1 = part1 exampleOrders exampleUpdates
    print [example1, example1 - 143]
    let answerPart1 = part1 inputOrders inputUpdates
    print [answerPart1, answerPart1 - 6051]

    putStrLn "\n--part2"
    let example2 = part2 exampleOrders exampleUpdates
    print [example2, example2 - 123]
    let answerPart2 = part2 inputOrders inputUpdates
    print [answerPart2, answerPart2 - 5093]