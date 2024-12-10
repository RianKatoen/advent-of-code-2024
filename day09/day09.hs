import Data.Char (digitToInt)
import Data.Bits (shiftR)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)
import Debug.Trace (traceShowId, traceShow)

-- Data.
data BitRange = BitRange { id :: Maybe Int, blocks :: Int }
    deriving (Show, Eq)

getId :: BitRange -> Maybe Int
getId (BitRange id _) = id

noBlocks :: BitRange -> Int
noBlocks (BitRange _ blocks) = blocks

-- Parse.
parseDisk :: [Char] -> [BitRange]
parseDisk = filter (\(BitRange _ s) -> s > 0) . zipWith (curry toBitRange) [0..] . map digitToInt
    where toBitRange (i, d) = BitRange (toId i) d
          toId       id     = if even id then Just (id `shiftR` 1) else Nothing

-- Challenge
(<~>) :: BitRange -> BitRange -> (BitRange, BitRange, BitRange)
(<~>) (BitRange a_id a_blocks) (BitRange b_id b_blocks)
        | isJust a_id   = (BitRange a_id a_blocks,     BitRange Nothing 0,            BitRange b_id b_blocks)
        | otherwise     = (BitRange b_id new_a_blocks, BitRange Nothing new_0_blocks, BitRange b_id new_b_blocks)
    where new_a_blocks  = min a_blocks b_blocks
          new_b_blocks  = max 0 $ b_blocks - new_a_blocks
          new_0_blocks  = max 0 $ a_blocks - new_a_blocks

(<:>) :: BitRange -> BitRange -> (BitRange, BitRange, BitRange)
(<:>) (BitRange a_id a_blocks) (BitRange b_id b_blocks)
        | not fits      = (BitRange a_id a_blocks,     BitRange Nothing 0,            BitRange b_id b_blocks)
        | isJust a_id   = (BitRange a_id a_blocks,     BitRange Nothing 0,            BitRange b_id b_blocks)
        | otherwise     = (BitRange b_id new_a_blocks, BitRange Nothing new_0_blocks, BitRange b_id new_b_blocks)
    where fits          = a_blocks >= b_blocks
          new_a_blocks  = min a_blocks b_blocks
          new_b_blocks  = max 0 $ b_blocks - new_a_blocks
          new_0_blocks  = max 0 $ a_blocks - new_a_blocks


compress :: [BitRange] -> [BitRange]
compress (a:rest)
        | null rest             = [a | isJust (getId a) && noBlocks a > 0]
        | isNothing (getId b)   = compress (a:middle)
        | noBlocks a == 0       = compress rest
        | noBlocks b == 0       = compress (a:middle)
        | a == na               = a : compress rest
        | otherwise             = na : compress (nz : middle ++ [nb])
    where (middle, b)   = (init rest, last rest)
          (na, nz, nb)    = a <~> b

printMemory :: [BitRange] -> String
printMemory = concat . concatMap (\(BitRange id blocks) -> replicate blocks (maybe "." show id))

part1 x = sum $ zipWith (*) [0.. ] $ concatMap (\(BitRange id blocks) -> replicate blocks $ fromMaybe 0 id) x


main = do
    let exampleInput1 = "12345"
    let exampleInput2 = "2333133121414131402"
    input <- readFile "input.txt"

    putStrLn "\npart 1"
    print $ printMemory . compress $ parseDisk exampleInput2
    print $ part1 . compress $ parseDisk exampleInput1
    print $ part1 . compress $ parseDisk exampleInput2
    print $ part1 . compress $ parseDisk input

    putStrLn "\npart 2"