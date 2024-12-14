import qualified Data.Bifunctor
import Debug.Trace (traceShowId, traceShow)
import Data.Bits (shiftR)
import Data.Foldable (for_)

data Robot = Robot { pos :: (Int, Int), vel :: (Int, Int) }
    deriving (Eq, Show)

parseRobots :: FilePath -> IO [Robot]
parseRobots filePath = do
        content <- readFile filePath
        let linez = map (break (==' ')) (lines content)
        return $ map (uncurry Robot . Data.Bifunctor.bimap parseLine parseLine) linez
    where parseLine = Data.Bifunctor.bimap (read @Int) (read @Int . tail) . break (==',') . tail . dropWhile (/= '=')

(<~>) x t   = if x < 0 then (t - (t - x) `rem` t) `rem` t else x `rem` t
(|+|) (Robot (p_c, p_r) (v_c, v_r)) z = Robot (p_c + v_c * z, p_r + v_r * z) (v_c, v_r)
(|/|) (Robot (p_c, p_r) (v_c, v_r)) (t_c, t_r) = Robot (p_c <~> t_c, p_r <~> t_r) (v_c, v_r)

part1 :: (Int, Int) -> Int -> [Robot] -> Int
part1 (c, r) seconds robots = product $ map (length . (`filter` newRobots)) quadrants
    where newRobots = map (\robot -> (robot |+| seconds) |/| (c, r)) robots
          (q_c, q_r) = (c `shiftR` 1, r `shiftR` 1)
          inQuadrant cc cr (Robot (c, r) _) = cc c && cr r
          quadrants = [inQuadrant (<q_c) (<q_r), inQuadrant (<q_c) (>q_r), inQuadrant (>q_c) (<q_r), inQuadrant (>q_c) (>q_r)]


printRobot (Robot (c, r) _) (t_c, t_r) = do
        zipWith (curry replaceRow) [0..] rows
    where rows                    = replicate t_r $ replicate t_c '.'
          replaceRow (i, row)     = if i == r then zipWith (curry replaceColumn) [0..] row else row
          replaceColumn (j, char) = if j == c then '#' else char

-- printRobots :: (Int, Int) -> [Robot] -> [[Char]]
printRobots (t_c, t_r) robots = map replaceRow rows
    where rows                          = take t_r $ map (, take t_c $ map (, '.') [0..]) [0..]
          anyRobotOnLoc (c, r)          = any (\(rc, rr) -> rc == c && rr == r) robots
          replaceRow (r, row)           = map (replaceColumn r) row
          replaceColumn r (c, char)     =  if anyRobotOnLoc (c, r) then '#' else char


moveRobots :: (Int, Int) -> [Robot] -> Int -> [Robot]
moveRobots tiles robots seconds = map (\robot -> (robot |+| seconds) |/| tiles) robots

main :: IO ()
main = do
    exampleRobots <- parseRobots "example1.txt"
    let exampleTiles = (11, 7)

    inputRobots <- parseRobots "input.txt"
    let inputTiles = (101, 103)

    putStrLn "\n--part 1"
    let example1 = part1 exampleTiles 100 exampleRobots
    print [example1, example1 - 12]
    let answerPart1 = part1 inputTiles 100 inputRobots
    print [answerPart1, answerPart1 - 218619324]

    putStrLn "\n--part 2"
    -- let stuff = take 1000 $ map ((\(s, robots) -> traceShow s (s, printRobots inputTiles robots)) . (\s -> (s, map (\(Robot (c, r) _) -> (c, r)) $ moveRobots inputTiles inputRobots (83 + 101 * s)))) [0..]
    -- for_ stuff (\(s, robots) -> mapM_ print ([show s, "---------------------------------------------"] ++ robots ++ ["", ""])
    
    mapM_ print $ printRobots inputTiles $ map (\(Robot a _) -> a) $ moveRobots inputTiles inputRobots (83 + 63 * 101)
