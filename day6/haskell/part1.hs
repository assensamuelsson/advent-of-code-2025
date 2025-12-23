import Data.List.Split (splitOn)
import Data.List (zipWith4)

parseInput :: String -> ([Integer], [Integer], [Integer], [(Integer -> Integer -> Integer)])
parseInput input = (xs, ys, zs, ops)
  where
    rows = map words $ lines input
    [xs, ys, zs] = map (map read) (take 3 rows)
    ops = map parseOp (rows !! 3)

parseOp :: String -> (Integer -> Integer -> Integer)
parseOp "+" = (+)
parseOp "*" = (*)
parseOp "-" = (-)
parseOp "/" = div

doCalc :: Integer -> Integer -> Integer -> (Integer -> Integer -> Integer) -> Integer
doCalc x y z op = foldl1 op [x, y, z]


main :: IO ()
main = do
    input <- getContents
    let (xs, ys, zs, ops) = parseInput input
    print xs
    print ys
    print zs
    let sums = sum $ zipWith4 doCalc xs ys zs ops
    print sums
