import Data.Bits (shiftR)
import qualified Data.Map.Strict as Map

rule :: String -> [String]
rule "0" = ["1"]
rule x
        | even (length x)          = clean $ splitAt (length x `shiftR` 1) x
        | otherwise                = [show (2024 * read @Int x)]
    where clean (x, y)  = [x, show (read @Int y)]

blink :: Map.Map String Int -> Map.Map String Int
blink = toMap . concatMap concatRule . Map.toList
    where concatRule (k, n) = map (, n) $ rule k
          toMap        = Map.fromListWithKey (\ k a b -> a + b)        

blinking :: Map.Map String Int -> Int -> Int
blinking stones = length . (iterate blink stones !!)

solution :: [String] -> Int -> Int
solution x n = sum $ map snd $ Map.toList $ iterate blink stones !! n
    where stones = Map.fromList $ map (, 1 :: Int) x

main = do
    let example =   ["125", "17"]
    let input =     ["554735", "45401", "8434", "0", "188", "7487525", "77", "7"]

    putStrLn "\npart 1"
    let example1 = solution example 25
    print [example1, example1 - 55312]
    let answerPart1 = solution input 25
    print [answerPart1, answerPart1 - 209412]

    putStrLn "\npart 2"
    let answerPart2 = solution input 75
    print [answerPart2, answerPart2 - 248967696501656]