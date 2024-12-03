import Data.Char (isDigit)
import Debug.Trace (traceShow)

-- Let's try and do this without regex...
split :: Char -> String -> [String]
split _ [] = [""]
split c x = do
    let until = takeUntil [c] x
    if length until >= length x
        then [until]
    else do
        let y = dropUntil [c] x
        until : split c y

isNumber :: String -> Bool
isNumber [] = False
isNumber [c] = isDigit c
isNumber x = all (\c -> isNumber [c]) x

isLegit :: String -> Bool
isLegit x
    | length x < 3 = False -- At least 1 digit
    | length x > 7 = False -- No more than 3 digits.
    | otherwise = all (\y -> isNumber y && not (null y) && (length y <= 3)) (split ',' x)

dropUntil :: String -> String -> String
dropUntil a x
    | length a > length x = []
    | length x == 1 = []
    | a == take (length a) x = drop (length a) x
    | otherwise = dropUntil a (tail x)

takeUntil :: String -> String -> String
takeUntil a x
    | length a > length x = x
    | a == take (length a) x = []
    | otherwise = take 1 x ++ takeUntil a (tail x)

capture :: String -> String -> String -> String
capture a b x = do
    let start = takeUntil a x
    let remainder = takeUntil b start
    if length start == length remainder
        then []
    else
        remainder

captureAll :: (String -> Bool) -> String -> String -> String -> [String]
captureAll f a b x
    | null x  = []
    | length a + length b >= length x = []
    | otherwise = do
        let captured = capture a b x
        if f captured
            then captured : captureAll f a b (dropUntil (a ++ captured ++ b) x)
        else
            captureAll f a b (dropUntil a x)

calculate :: [String] -> Int
calculate x = product $ map read x

part1 :: String -> Int
part1 x = sum $ map (calculate . split ',') (captureAll isLegit "mul(" ")" x)

part2 :: String -> Int
part2 x = sum $ map part1 $ captureAll (const True) "do()" "don't()" ("do()" ++ x ++ "don't()")

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