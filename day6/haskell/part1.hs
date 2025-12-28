parseInput :: String -> ([[Integer]], [(Integer -> Integer -> Integer)])
parseInput input = (numberRows, ops)
  where
    rows = map words $ lines input
    numberRows = map (map read) (init rows)
    ops = map parseOp (last rows)

parseOp :: String -> (Integer -> Integer -> Integer)
parseOp "+" = (+)
parseOp "*" = (*)

doCalc :: [Integer] -> (Integer -> Integer -> Integer) -> Integer
doCalc numbers op = foldl1 op numbers

main :: IO ()
main = do
    input <- getContents
    let (numberRows, ops) = parseInput input
    let transposed = transpose numberRows
    let results = zipWith doCalc transposed ops
    print (sum results)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose xs = map head xs : transpose (map tail xs)