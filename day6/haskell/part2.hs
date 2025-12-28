main :: IO ()
main = do
    input <- getContents
    let rows = parseInput input
    let transposed = transpose rows
    let filtered = filter (not . all (== ' ')) transposed
    let result = foldl part2 ([], 0) filtered
    print result

parseInput :: String -> [String]
parseInput input = map reverse $ lines input

isOp :: Char -> Bool
isOp c = c == '+' || c == '*'

parseOp :: Char -> (Integer -> Integer -> Integer)
parseOp '+' = (+)
parseOp '*' = (*)

doCalc :: [Integer] -> (Integer -> Integer -> Integer) -> Integer
doCalc numbers op = foldl1 op numbers

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose xs = map head xs : transpose (map tail xs)

part2 :: ([Integer], Integer) -> String -> ([Integer], Integer)
part2 st [] = st
part2 (stack, acc) row =
    let digitPart     = init row
        op            = last row
        numberToStack = read digitPart : stack
        newStack      = if isOp op then [] else numberToStack
        newAcc        = if isOp op then acc + doCalc numberToStack (parseOp op) else acc
    in (newStack, newAcc)
