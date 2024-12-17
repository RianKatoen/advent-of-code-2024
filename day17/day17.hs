import Data.Maybe (fromMaybe, fromJust, isJust)
import qualified Data.Bifunctor
import Data.Bits (xor)

combo :: Int -> (Int, Int, Int) -> Int
combo op (a, b, c)
    | op == 0   = 0
    | op == 1   = 1
    | op == 2   = 2
    | op == 3   = 3
    | op == 4   = a
    | op == 5   = b
    | op == 6   = c
    | otherwise = error "Invalid combo operand"

instruction :: (Int, Int) -> (Int, Int, Int) -> ((Int, Int, Int), Maybe Int, Maybe Int)
instruction (op, co) (a, b, c)
        | op == 0   =  ((a `div` (2 ^ coop), b, c),   Nothing, Nothing)
        | op == 1   =  ((a, b `xor` co, c),                  Nothing, Nothing)
        | op == 2   =  ((a, coop `rem` 8, c),                Nothing, Nothing)
        | op == 3   =  ((a, b, c), if a == 0 then Nothing else Just co, Nothing)
        | op == 4   =  ((a, b `xor` c, c),                   Nothing, Nothing)
        | op == 5   =  ((a, b, c),                           Nothing, Just $ coop `rem` 8)
        | op == 6   =  ((a, a `div` (2 ^ coop), c),   Nothing, Nothing)
        | op == 7   =  ((a, b, a `div` (2 ^ coop)),   Nothing, Nothing)
    where coop = combo co (a, b, c)

run :: (Int, Int, Int) -> Int -> [Int] -> ((Int, Int, Int), [Int])
run (a, b, c) pointer ics
        | length ics < pointer + 1          = ((a, b, c), [])
        | otherwise                         = Data.Bifunctor.second (out ++) $ run (aa, bb, cc) newPointer ics
    where (op, co)                                = (ics !! pointer, ics !! (pointer + 1))
          ((aa, bb, cc), maybePointer, maybeOut)  = instruction (op, co) (a, b, c)
          newPointer                              = fromMaybe (pointer + 2) maybePointer
          out                                     = [fromJust maybeOut | isJust maybeOut]

main = do
    putStrLn "\nexamples"
    print $ run (0, 0, 9)         0 [2, 6]
    print $ run (10, 0, 0)        0 [5,0,5,1,5,4]
    print $ run (2024, 0, 0)      0 [0,1,5,4,3,0]
    print $ run (0, 29, 0)        0 [1,7]
    print $ run (0, 2024, 43690)  0 [4,0]

    putStrLn "\nexample 1"
    print $ run (729, 0, 0)       0 [0,1,5,4,3,0]

    putStrLn "\npart 1"
    let input = [2,4,1,2,7,5,4,5,0,3,1,7,5,5,3,0]
    print $ run (22817223, 0, 0)  0 input

    putStrLn "\nexample 2"
    print $ run (117440, 0, 0)    0 [0,3,5,4,3,0]
    
    putStrLn "\npart 2: history" 
    -- manual periodication.
    print $ run (0, 0, 0)  0 input
    print $ run (2, 0, 0)  0 input
    print $ run (8, 0, 0)  0 input
    print $ run (64, 0, 0)  0 input
    print $ run (512, 0, 0)  0 input
    print $ run (4096, 0, 0)  0 input
    print $ run (32768, 0, 0)  0 input
    print $ run (35184372088832, 0, 0)  0 input
    print $ run (281474976710655, 0, 0)  0 input
    
    putStrLn "\npart 2"
    let wut = 5 * 35184372088832 + 3 * 4398046511104 + 2 * 549755813888 + 2 * 68719476736 + 3 * 8589934592 + 5 * 1073741824 + 3 * 134217728 + 7 * 16777216 + 2 * 2097152 + 7 * 262144 + 2 * 32768 + 3 * 4096 + 6 * 512   
    mapM_ print $ takeWhile (\(a, (_, b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:_)) -> b /= 2 || c /= 4 || d /= 1 || e /= 2 || f /= 7 || g /= 5 || h /= 4 || i /= 5 || j /= 0 || k /= 3 || l /= 1 || m /= 7 || n /= 5 || o /= 5 || p /= 3) $ map (\a -> (wut + (a + 1), run (wut + a, 0, 0) 0 input)) [1..]
    print $ snd $ run (190384615275535, 0, 0) 0 input