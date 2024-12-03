import Data.List (sort)

parseLine :: (String, String) -> (Int, Int)
parseLine (a, b) = (read a :: Int, read b :: Int)

getInput :: FilePath -> IO ([Int], [Int])
getInput filePath = do
  contents <- readFile filePath
  let input = lines contents
  return $ unzip $ map (parseLine . break (==' ')) input

getPart1 :: [Int] -> [Int] -> Int
getPart1 a b = sum $ zipWith (\x y -> abs (x - y)) (sort a) (sort b)

getPart2 :: [Int] -> [Int] -> Int
getPart2 a b = sum $ map (\x -> x * length (filter (==x) b)) a

main = do
  (exampleList1, exampleList2)  <- getInput "example.txt"
  (list1, list2)                <- getInput "input.txt"
  
  -- part1
  putStrLn "\npart1"
  let example1 = getPart1 exampleList1 exampleList2
  print [example1, example1 - 11]
  let part1 = getPart1 list1 list2
  print [part1, part1 - 2344935]

  -- part2
  putStrLn "\npart2"
  let example2 = getPart2 exampleList1 exampleList2
  print [example2, example2 - 31]
  let part2 = getPart2 list1 list2
  print [part2, part2 - 27647262]