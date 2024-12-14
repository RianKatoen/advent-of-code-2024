import Debug.Trace (traceShowId)
import Data.List (groupBy)
import Data.Maybe (isJust, fromJust)
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

-- (<-->) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(<-->) (a, b) (x, y) = (a - x, b - y)
(<//>) (a, b) (x, y) = (a `div` x, b `div` y)
(</=>) (a, b) (x, y) = a /= x || b /= y
(<<<) (a, b) (x, y) = a <= x && b <= y
(<**>) (a, b) (x, y) = (a * x, b * y)

(<-*->) z (a, b) = (a * z, b * z)
(<-<-<) (a, b) z = a < z && b < z

-- solveB :: Arcade -> Int -> Maybe Int
getPossibleA :: Arcade -> [Int]
getPossibleA (Arcade a _ p) = takeWhile (\n -> n <-*-> a <<< p) [0..]

solveB :: Arcade -> Int -> Maybe Int
solveB (Arcade a b p) n_a
        | rem <-<-< 0                               = Nothing
        | uncurry (/=) n_b                          = Nothing
        | (rem <--> (fst n_b <-*-> b)) </=> (0, 0)   = Nothing
        | otherwise                                 = Just $ fst n_b
    where rem   = p <--> (n_a <-*-> a)
          n_b   = rem <//> b

getPossibleSolutions :: Arcade -> [(Int, Int)]
getPossibleSolutions arcade = map (Data.Bifunctor.second fromJust) $ filter (\(_, b) ->  isJust b) $ map (\n_a -> (n_a, solveB arcade n_a)) $ getPossibleA arcade

-- part1 :: [Arcade] -> Int
part1 = sum . map (uncurry (+) . (<**> (3, 1)) . head) . filter (/=[]) . map getPossibleSolutions

main = do
    exampleArcades <- parseArcades "example1.txt"
    inputArcades <- parseArcades "input.txt"

    let example1 = part1 exampleArcades
    print [example1, example1 - 480]
    let answerPart1 = part1 inputArcades
    print [answerPart1, answerPart1 - 480]