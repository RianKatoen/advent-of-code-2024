import qualified Data.Bifunctor
import Distribution.Utils.String (trim)
import Data.List (isPrefixOf, sort)
import Debug.Trace (traceShowId)

split :: Char -> String -> [String]
split c s = case rest of
        []     -> [chunk]
        _:rest -> chunk : split c rest
    where (chunk, rest) = break (==c) s

parseTowels :: FilePath -> IO ([String], [String])
parseTowels filePath = do
    contents <- readFile filePath
    let linez = (\x -> (head x, drop 2 x)) $ lines contents
    return $ Data.Bifunctor.first (map trim . split ',') linez


isTowelPossible :: [String] -> String -> Bool
isTowelPossible towels []            = True
isTowelPossible towels requiredTowel 
        | null prefixes = False
        | otherwise     = any (isTowelPossible towels) newRequiredTowels
    where prefixes          = filter (`isPrefixOf` requiredTowel) towels
          newRequiredTowels = map (\x -> drop (length x) requiredTowel) prefixes


dedupTowels :: [String] -> [String]
dedupTowels towels = filter (not . dedupTowel) towels
    where dedupTowel t = isTowelPossible (filter (/=t) towels) t

part1 :: ([String], [String]) -> Int
part1 (towels, requiredTowels) = length $ filter (isTowelPossible dedupedTowels) requiredTowels
    where dedupedTowels = dedupTowels towels

main = do
    putStrLn "\npart 1"
    parseTowels "example.txt" >>= print . part1
    parseTowels "input.txt" >>= print . part1