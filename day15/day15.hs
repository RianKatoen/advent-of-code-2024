import Data.List (elemIndex)
import Data.Maybe (mapMaybe, isNothing, fromJust)
import qualified Control.Arrow as Data.Bifunctor

parseFile filePath = do
        contents <- readFile filePath
        let (warehouse, moves) = break (=="") $ lines contents
        let loc = head . mapMaybe checkLine $ zip [0..] warehouse
        return (loc, map (map (\s -> if s == '@' then '.' else s)) warehouse, concat $ tail moves)
    where whereIsRobot      = elemIndex '@'
          checkLine (x, y)  = if isNothing (whereIsRobot y) then Nothing else Just (x, fromJust $ whereIsRobot y)

transform :: [[Char]] -> [[Char]]
transform = map (concatMap transform')
    where transform' '#' = "##"
          transform' 'O' = "[]"
          transform' '.' = ".."

direction :: Char -> (Int, Int)
direction d = case d of
    '<'   -> (0, -1)
    '>'   -> (0,  1)
    '^'   -> (-1, 0)
    'v'   -> ( 1, 0)

(<++>) (a, b) (x, y) = (a + x, b + y)
(<**>) (a, b) z = (a * z, b * z)

getObj :: [[Char]] -> (Int, Int) -> Char
getObj warehouse (r, c) = warehouse !! r !! c

getObjs :: [[Char]] -> (Int, Int) -> Bool -> [(Char, (Int, Int))]
getObjs warehouse (r, c) sideways
        | obj == 'O' = [('O', (r, c))]
        | obj == ']' = (']', (r, c)) : ([('[', left) | not sideways])
        | obj == '[' = ('[', (r, c)) : ([(']', right) | not sideways])
    where obj = warehouse !! r !! c
          left = (r, c) <++> direction '<'
          right = (r, c) <++> direction '>'

move :: (Int, Int) -> Char -> (Int, Int)
move loc d = loc <++> direction d

moveRobot :: ([[Char]], (Int, Int)) -> Char -> ([[Char]], (Int, Int))
moveRobot (warehouse, loc) d
        | nextWarehouse !! r !! c == '.' = (nextWarehouse, (r, c))
        | warehouse     !! r !! c == '#' = (warehouse, loc)
        | otherwise                      = (warehouse, loc)
    where dir           = direction d
          (r, c)        = loc <++> dir
          nextWarehouse = moveCrate warehouse (r, c) dir

replace :: [[Char]] -> [(Char, (Int, Int))] -> [[Char]]
replace warehouse [(char, (r, c))] = zipWith (\rr row -> if rr /= r then row else zipWith (\cc o -> if cc /= c then o else char) [0..] row) [0..] warehouse
replace warehouse ((char, (r, c)):rest) = replace (replace warehouse [(char, (r, c))]) rest

isObj :: Char -> Bool
isObj o = case o of
    'O' -> True
    '[' -> True
    ']' -> True
    _   -> False

iteration :: ([[Char]] -> (Int, Int) -> [[Char]]) -> [[Char]] -> [(Int, Int)] -> [[Char]]
iteration f x [y] = f x y
iteration f x (y:rest) = iteration f (f x y) rest

moveCrate :: [[Char]] -> (Int, Int) -> (Int, Int) -> [[Char]]
moveCrate warehouse (r, c) dir
        | not (isObj obj)                        = warehouse
        | any ((== '#') . fst) newObjs           = warehouse
        | all ((== '.') . fst) newObjs           = moveCrate placeCrate (r, c) dir
        | any isObj newNewObj                    = warehouse
        | otherwise                              = moveCrate nextCrates (r, c) dir
    where obj           = getObj warehouse (r, c)
          objs          = getObjs warehouse (r, c) (dir == (0, 1) || dir == (0, -1))
          newLocs       = map ((<++> dir) . snd) objs
          newObjs       = map (\x -> (getObj warehouse x, x)) newLocs
          placeCrate    = replace (replace warehouse $ map (\(_, y) -> ('.', y)) objs) $ zip (map fst objs) newLocs
          nextCrates    = iteration (\x y -> moveCrate x y dir) warehouse newLocs
          newNewObj     = map (getObj nextCrates) newLocs

doMoves :: ([[Char]], (Int, Int)) -> [Char] -> ([[Char]], (Int, Int))
doMoves (warehouse, loc) [c]      = moveRobot (warehouse, loc) c
doMoves (warehouse, loc) (c:rest) = doMoves (moveRobot (warehouse, loc) c) rest

printWarehouse :: ([[Char]], (Int, Int)) -> [[Char]]
printWarehouse (warehouse, loc) = zipWith (\r w -> zipWith (\c cc -> if (r, c) == loc then '@' else cc) [0..] w) [0..] warehouse

part1 :: ([[Char]], (Int, Int)) -> [Char] -> Int
part1 (warehouse, loc) moves = do
    let (finalWarehouse, _) = doMoves (warehouse, loc) moves
    sum $ concat $ zipWith (\r row -> zipWith (\c cc -> if cc == 'O' || cc == '[' then 100 * r + c else 0) [0..] row) [0..] finalWarehouse

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

    putStrLn "\nexample"
    (exampleLoc3, exampleWarehouse3, exampleMoves3) <- parseFile "example3.txt"
    let (exampleWarehousez3, exampleLocz3) = (transform exampleWarehouse3, Data.Bifunctor.second (2 *) exampleLoc3)
    mapM_ print $ printWarehouse $ doMoves (exampleWarehousez3, exampleLocz3) exampleMoves3

    putStrLn ""
    let (exampleWarehousez2, exampleLocz2) = (transform exampleWarehouse2, Data.Bifunctor.second (2 *) exampleLoc2)
    mapM_ print $ printWarehouse $ doMoves (exampleWarehousez2, exampleLocz2) exampleMoves2

    putStrLn "\npart2"
    let example22 = part1 (exampleWarehousez2, exampleLocz2) exampleMoves2
    print [example22, example22 - 9021]

    let (warehousez, locz) = (transform warehouse, Data.Bifunctor.second (2 *) loc)
    let answerPart2 = part1 (warehousez, locz) moves
    print [answerPart2, answerPart2 - 1404917]