import Data.Maybe (fromMaybe, fromJust, isJust)
import qualified Data.Bifunctor
import Data.Bits (xor)

combo :: Int -> (Int, Int, Int) -> Int
combo 0 _ = 0
combo 1 _ = 1
combo 2 _ = 2
combo 3 _ = 3
combo 4 (a, _, _) = a
combo 5 (_, b, _) = b
combo 6 (_, _, c) = c

instruction :: Int -> Int -> (Int, Int, Int) -> ((Int, Int, Int), Maybe Int, Maybe Int)
instruction 0 op (a, b, c) = ((a `div` (2 ^ combo op (a,b,c)), b, c), Nothing, Nothing)
instruction 1 op (a, b, c) = ((a, b `xor` op, c), Nothing, Nothing)
instruction 2 op (a, b, c) = ((a, combo op (a,b,c) `rem` 8, c), Nothing, Nothing)
instruction 3 op (a, b, c) = ((a,b,c), if a == 0 then Nothing else Just op, Nothing)
instruction 4 _  (a, b, c) = ((a, b `xor` c, c), Nothing, Nothing)
instruction 5 op (a, b, c) = ((a, b, c), Nothing, Just $ combo op (a,b,c) `rem` 8)
instruction 6 op (a, b, c) = ((a, a `div` (2 ^ combo op (a,b,c)), c),   Nothing, Nothing)
instruction 7 op (a, b, c) = ((a, b, a `div` (2 ^ combo op (a,b,c))),   Nothing, Nothing)

run :: (Int, Int, Int) -> Int -> [Int] -> ((Int, Int, Int), [Int])
run (a, b, c) pointer ics
        | length ics < pointer + 1          = ((a, b, c), [])
        | otherwise                         = Data.Bifunctor.second (out ++) $ run (aa, bb, cc) newPointer ics
    where (opcode, op)                            = (ics !! pointer, ics !! (pointer + 1))
          ((aa, bb, cc), maybePointer, maybeOut)  = instruction opcode op (a, b, c)
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
    -- let wut = 5 * 35184372088832 + 3 * 4398046511104 + 2 * 549755813888 + 2 * 68719476736 + 3 * 8589934592 + 5 * 1073741824 + 3 * 134217728 + 7 * 16777216 + 2 * 2097152 + 7 * 262144 + 2 * 32768 + 3 * 4096 + 6 * 512
    -- mapM_ print $ takeWhile (\(a, (_, b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:_)) -> b /= 2 || c /= 4 || d /= 1 || e /= 2 || f /= 7 || g /= 5 || h /= 4 || i /= 5 || j /= 0 || k /= 3 || l /= 1 || m /= 7 || n /= 5 || o /= 5 || p /= 3) $ map (\a -> (wut + (a + 1), run (wut + a, 0, 0) 0 input)) [1..]
    print $ snd $ run (190384615275535, 0, 0) 0 input