import Data.Char (digitToInt)
import Data.Bits (shiftR)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)
import Data.List (findIndex)

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

(<+>) :: BitRange -> BitRange -> (BitRange, BitRange, BitRange)
(<+>) (BitRange a_id a_blocks) (BitRange b_id b_blocks)
        | isJust b_id   = (BitRange a_id a_blocks,     BitRange Nothing 0,            BitRange b_id b_blocks)
        | otherwise     = (BitRange a_id new_a_blocks, BitRange Nothing new_0_blocks, BitRange a_id new_b_blocks)
    where new_b_blocks  = min b_blocks a_blocks
          new_a_blocks  = max 0 $ a_blocks - new_b_blocks
          new_0_blocks  = max 0 $ b_blocks - new_b_blocks

(<:>) :: BitRange -> BitRange -> ([BitRange], [BitRange])
(<:>) (BitRange a_id a_blocks) (BitRange b_id b_blocks)
        | isNothing a_id || isJust b_id || not fits
            = ([BitRange a_id a_blocks],     [BitRange b_id b_blocks])
        | otherwise
            = ([BitRange Nothing a_blocks], [BitRange Nothing new_0_blocks | new_0_blocks > 0] ++ [BitRange a_id new_b_blocks])
    where fits          = a_blocks <= b_blocks
          new_b_blocks  = min a_blocks b_blocks
          new_a_blocks  = max 0 $ a_blocks - new_b_blocks
          new_0_blocks  = max 0 $ b_blocks - new_b_blocks

compross [a] = [a]
compross ((BitRange id_a size_a):brs)
        | isNothing id_a                    = BitRange id_a size_a : compross brs
        | isNothing maybeIx                 = BitRange id_a size_a : compross brs
        | BitRange id_a size_a:brs == next  = next
        | otherwise                         = compross next

    where srb               = reverse brs
          maybeIx           = findIndex (\(BitRange id_b size_b) -> isNothing id_b && size_b >= size_a) srb
          ix                = fromJust maybeIx
          (before, after)   = splitAt ix srb
          (na, nb)          = BitRange id_a size_a <:> (srb !! ix)
          next              = reverse $ before ++ reverse nb ++ tail after ++ na


printMemory :: [BitRange] -> String
printMemory = concat . concatMap (\(BitRange id blocks) -> replicate blocks (maybe "." show id))

score :: Foldable t => t BitRange -> Int
score = sum . zipWith (*) [0.. ] . concatMap (\(BitRange id blocks) -> replicate blocks $ fromMaybe 0 id)

part1 :: String -> Int
part1 = score . compress . parseDisk

part2 :: String -> Int
part2 = score . reverse . compross . reverse . parseDisk

main = do
    let exampleInput1 = "12345"
    let exampleInput2 = "2333133121414131402"
    input <- readFile "input.txt"

    putStrLn "\npart 1"
    let example11 = part1 exampleInput1
    print [example11, example11 - 60]
    let example12 =  part1 exampleInput2
    print [example12, example12 - 1928]
    let answerPart1 = part1 input
    print [answerPart1, answerPart1 - 6607511583593]

    putStrLn "\npart 2"
    let example2 =  part2 exampleInput2
    print [example2, example2 - 2858]
    let answerPart2 = part2 input
    print [answerPart2, answerPart2 - 6636608781232]