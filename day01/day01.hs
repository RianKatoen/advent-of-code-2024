import System.IO (readFile)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

parseLine :: String -> Maybe (Int, Int)
parseLine line =
    case words line of
        [x, y] -> (,) <$> readMaybe x <*> readMaybe y
        _      -> Nothing

readFileAndParse :: FilePath -> IO ([Int], [Int])
readFileAndParse filePath = do
    contents <- readFile filePath
    let (list1, list2) = unzip (mapMaybe parseLine (lines contents))
    return (list1, list2)

countInList :: [Int] -> Int -> Int
countInList list x  = do
    x * length (filter (==x) list)

main :: IO ()
main = do
    (list1, list2) <- readFileAndParse "input.txt"

    -- part1
    putStrLn "\npart1"
    print $ sum $ zipWith (\x y -> abs (x - y)) (sort list1) (sort list2)

    -- part2
    putStrLn "\npart2"
    print $ sum $ map (countInList list2) list1