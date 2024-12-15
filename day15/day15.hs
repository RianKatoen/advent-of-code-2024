import Data.List (elemIndex)
import Data.Maybe (mapMaybe, isNothing, fromJust)

parseFile filePath = do
        contents <- readFile filePath
        let (warehouse, moves) = break (=="") $ lines contents
        let loc = head . mapMaybe checkLine $ zip [0..] warehouse
        return (loc, map (map (\s -> if s == '@' then '.' else s)) warehouse, concat $ tail moves)
    where whereIsRobot      = elemIndex '@'
          checkLine (x, y)  = if isNothing (whereIsRobot y) then Nothing else Just (x, fromJust $ whereIsRobot y)

direction :: Char -> (Int, Int)
direction d = case d of
    '<'   -> (0, -1)
    '>'   -> (0,  1)
    '^'   -> (-1, 0)
    'v'   -> ( 1, 0)

(<++>) (a, b) (x, y) = (a + x, b + y)
(<**>) (a, b) z = (a * z, b * z)

move :: (Int, Int) -> Char -> (Int, Int)
move loc d = loc <++> direction d

inBounds :: Int -> (Int, Int) -> Bool
inBounds n (r, c) = r > 0 && r < n && c > 0 && c < n

filteredMove :: Int -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
filteredMove n loc d = if inBounds n m then Just m else Nothing
    where m = loc <++> d


moveRobot :: Int -> ([[Char]], (Int, Int)) -> Char -> ([[Char]], (Int, Int))
moveRobot n (warehouse, loc) d
        | warehouse     !! r !! c == '#' = (warehouse, loc)
        | nextWarehouse !! r !! c == '.' = (nextWarehouse, (r, c))
        | otherwise                      = (warehouse, loc)
    where dir           = direction d
          (r, c)        = loc <++> dir
          nextWarehouse = moveCrate warehouse (r, c) dir

replace :: [[Char]] -> Char -> (Int, Int) -> [[Char]]
replace warehouse rc (r, c) = zipWith (\rr row -> if rr /= r then row else zipWith (\cc o -> if cc /= c then o else rc) [0..] row) [0..] warehouse

moveCrate :: [[Char]] -> (Int, Int) -> (Int, Int) -> [[Char]]
moveCrate warehouse (r, c) dir
        | warehouse !! r !! c /= 'O'             = warehouse
        | newObj == '#'                          = warehouse
        | newObj == '.'                          = moveCrate placeCrate (r, c) dir
        | newNewObj == 'O'                       = warehouse
        | otherwise                              = moveCrate nextCrates (r, c) dir
    where (nr, nc)      = (r, c) <++> dir
          newObj        = warehouse !! nr !! nc
          nextCrates    = moveCrate warehouse (nr, nc) dir
          newNewObj     = nextCrates !! nr !! nc
          placeCrate    = replace (replace warehouse '.' (r, c)) 'O' (nr, nc)


doMoves :: Int -> ([[Char]], (Int, Int)) -> [Char] -> ([[Char]], (Int, Int))
doMoves n (warehouse, loc) [c]      = moveRobot n (warehouse, loc) c
doMoves n (warehouse, loc) (c:rest) = doMoves n (moveRobot n (warehouse, loc) c) rest

printWarehouse :: ([[Char]], (Int, Int)) -> [[Char]]
printWarehouse (warehouse, loc) = zipWith (\r w -> zipWith (\c cc -> if (r, c) == loc then '@' else cc) [0..] w) [0..] warehouse

part1 :: ([[Char]], (Int, Int)) -> [Char] -> Int
part1 (warehouse, loc) moves = do
    let (finalWarehouse, _) = doMoves (length warehouse) (warehouse, loc) moves
    sum $ concat $ zipWith (\r row -> zipWith (\c cc -> if cc == 'O' then 100 * r + c else 0) [0..] row) [0..] finalWarehouse

main = do
    (exampleLoc1, exampleWarehouse1, exampleMoves1) <- parseFile "example1.txt"
    (exampleLoc2, exampleWarehouse2, exampleMoves2) <- parseFile "example2.txt"
    (loc, warehouse, moves) <- parseFile "input.txt"
    
    putStrLn "\npart 1"
    let example11 = part1 (exampleWarehouse1, exampleLoc1) exampleMoves1
    print [example11, example11 - 2028]
    let example12 = part1 (exampleWarehouse2, exampleLoc2) exampleMoves2
    print [example12, example12 - 10092]
    let answerPart1 = part1 (warehouse, loc) moves
    print [answerPart1, answerPart1 - 1426855]