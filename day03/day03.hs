import Data.Char (isNumber)
import Data.List (isPrefixOf, findIndex, tails)
import Data.Maybe (isNothing, fromJust)

-- Let's try and do this without regex...
split :: Char -> String -> [String]
split c s = case rest of
        []     -> [chunk]
        _:rest -> chunk : split c rest
    where (chunk, rest) = break (==c) s

isLegitMul :: String -> Bool
isLegitMul x = case xs of
        [a, b] -> all isNumber a && all isNumber b
        _ -> False
    where xs = split ',' x

calculate :: [String] -> Int
calculate x = product $ map read x

capture :: (String -> Bool) -> String -> String -> String -> [String]
capture f a b x = do
    let ix_a = findIndex (isPrefixOf a) (tails x)
    case ix_a of
        Nothing -> []
        __ -> do
            let remainder = drop (fromJust ix_a + length a) x
            let ix_b = findIndex (isPrefixOf b) (tails remainder)
            case ix_b of
                Nothing -> []
                _ -> do
                    let captured = take (fromJust ix_b) remainder
                    if f captured
                        then captured : capture f a b (drop (fromJust ix_b) remainder)
                    else
                        capture f a b remainder

part1 :: String -> Int
part1 x = sum $ map (calculate . split ',') (capture isLegitMul "mul(" ")" x)

part2 :: String -> Int
part2 x = sum $ map part1 $ capture (const True) "do()" "don't()" ("do()" ++ x ++ "don't()")

main = do
    let exampleInput1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    let exampleInput2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

    inputFile <- readFile "input.txt"
    let input = head $ lines inputFile

    putStrLn "\n--part1"
    let example1 = part1 exampleInput1
    print [example1, example1 - 161]
    let answerPart1 = part1 input
    print [answerPart1, answerPart1 - 159833790]

    putStrLn "\n--part2"
    let example2 = part2 exampleInput2
    print [example2, example2 - 48]
    let answerPart2 = part2 input
    print [answerPart2, 89349241 - answerPart2]