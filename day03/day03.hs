import Debug.Trace (traceShow)
import Distribution.Simple.Setup (falseArg)
import Data.Char (isDigit)
import Control.Arrow (Arrow(first))
import Language.Haskell.TH (Lit(IntegerL))
import Distribution.Compat.Prelude (readMaybe)

count :: Char -> String -> Int
count c x = sum $ map (const 1) (filter (==c) x)

containsAny :: [Char] -> Char -> Bool
containsAny a x = x `elem` a

-- Checks if e
split :: Char -> String -> [String]
split _ [] = [""]
split c x = do
    let until = takeUntil [c] x
    if length until >= length x
        then [until]
    else do
        let y = dropUntil [c] x
        until : split c y


takeFrom :: Int -> Int -> [a] -> [a]
takeFrom a b xs = take b $ drop a xs

-- Let's try and do this without regex...
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
capture a b x
    | dropUntil a x == takeUntil b (dropUntil a x) = []
    | otherwise = takeUntil b (dropUntil a x)


captureAll :: String -> String -> String -> [String]
captureAll a b x = do
    let captured = capture a b x
    if null captured
        then []
    else do
        captured : (captureAll a b (dropUntil (a ++ captured ++ b) x) ++ captureAll a b (captured ++ b))

isNumber :: String -> Bool
isNumber [] = False
isNumber [c] = isDigit c
isNumber x = all (\c -> isNumber [c]) x

isLegit :: String -> Bool
isLegit x
    | null x = False
    | count ',' x /= 1 = False
    | length (split ',' x)  /= 2 = False
    | otherwise = all (\y -> isNumber y && (length y <= 3)) (split ',' x)

captureWithDoDont :: String -> [String]
captureWithDoDont x = do
    let start = takeUntil "don't()" x
    let remainder = dropUntil "don't()" x
    if null remainder 
        then [start]
    else do
        let next = dropUntil "do()" remainder
        if null next 
            then [start]
        else
            start : captureWithDoDont next

calculate :: [String] -> Int
calculate x = product $ map read x

part1 :: String -> Int
part1 x = sum $ map (calculate . split ',') (filter isLegit $ captureAll "mul(" ")" x)

part2 :: String -> Int
part2 x = sum $ map part1 (captureWithDoDont x)

main = do
    let exampleInput1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    let exampleInput2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

    inputFile <- readFile "input.txt"
    let input = lines inputFile

    putStrLn "\n--part1"
    let example1 = part1 exampleInput1
    print [example1, example1 - 161]
    let answerPart1 = sum $ map part1 input
    print [answerPart1, answerPart1 - 159833790]

    putStrLn "\n--part2"
    -- print $ isLegit "2,"
    -- print $ isLegit "2,5"
    -- print $ isLegit "2,544"
    -- print $ isLegit "2,54455"
    -- print $ isLegit ",0"
    -- print $ isLegit "1000,10000"
    let example2 = part2 exampleInput2
    print [example2, example2 - 48]
    let answerPart2 = sum $ map part2 input
    print [answerPart2, 91634027 - answerPart2]

    -- print $ captureWithDoDont exampleInput2
    -- print $ part1 (takeUntil "do()" $ dropUntil "don't()" exampleInput2)