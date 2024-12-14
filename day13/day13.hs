import Debug.Trace (traceShowId)
import Data.List (groupBy)
import Data.Maybe (isJust, fromJust, mapMaybe)
import qualified Data.Bifunctor

data Arcade = Arcade { a :: (Int, Int), b :: (Int, Int), prize :: (Int, Int) }
    deriving (Show, Eq)

parseArcades :: FilePath -> IO [Arcade]
parseArcades filePath = do
        content <- readFile filePath
        let linez = filter (/="") $ lines content
        return $ map (parseArcade . map snd) . groupBy (\(i, _) (j, _) -> i == j) $ zip (map (`div` 3) [0..]) linez
    where parseArcade l = Arcade (parseLine (head l)) (parseLine (l !! 1)) (parseLine (l !! 2))

parseLine :: String -> (Int, Int)
parseLine str
        | moves      <- tail (dropWhile (/= ':') str),
          (x, y)     <- break (==',') moves
         = (parseMove x, parseMove y)
    where parseMove = read @Int . tail . dropWhile (\c -> c /= '+' && c /= '=')

(<-->) (a, b) (x, y) = (a - x, b - y)
(</=>) (a, b) (x, y) = a /= x || b /= y

solve :: Arcade -> Maybe (Int, Int)
solve (Arcade (a_x, a_y) (b_x, b_y) (p_x, p_y))
        | ((p_x, p_y) <--> (a_x * n_a + b_x * n_b, a_y * n_a + b_y * n_b)) </=> (0, 0) = Nothing
        | otherwise                                                                    = Just (n_a, n_b)
    where d          = a_x * b_y - b_x * a_y
          (n_a, n_b) = ((b_y * p_x - b_x * p_y) `div` d, (a_x * p_y - a_y * p_x) `div` d)

part1 :: [Arcade] -> Int
part1 = sum . map ((uncurry (+) . (<**> (3, 1)) . fromJust) . Just) . mapMaybe solve
    where (<**>) (a, b) (x, y) = (a * x, b * y)

part2 :: [Arcade] -> Int
part2 = part1 . map correctPrize
    where correctPrize (Arcade a b p) = Arcade a b ((<-+->) 10_000_000_000_000 p)
          (<-+->) z (a, b) = (a + z, b + z)

main = do
    exampleArcades <- parseArcades "example1.txt"
    inputArcades <- parseArcades "input.txt"

    putStrLn "\npart 1"
    let example1 = part1 exampleArcades
    print [example1, example1 - 480]
    let answerPart1 = part1 inputArcades
    print [answerPart1, answerPart1 - 35997]

    putStrLn "\npart 2"
    let example2 = part2 exampleArcades
    print [example2, example2 - 875318608908]
    let answerPart2 = part2 inputArcades
    print [answerPart2, answerPart2 - 82510994362072]