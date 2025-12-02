-- 6430 too high
-- 6300 too low

main :: IO ()
main = do
    input <- getContents
    let twists = map parseLine $ lines input
    let states = scanl applyMoveCountZeros (50, 0) twists
    print states

parseLine :: String -> Int
parseLine ('L':rest) = -(read rest)
parseLine ('R':rest) = read rest
parseLine _          = error "Invalid input"

applyMoveCountZeros :: (Int, Int) -> Int -> (Int, Int)
applyMoveCountZeros (pos, count) delta = (newPosTruncated, newCount)
    where newPos = pos + delta
          wrapAt = 100
          newPosTruncated = newPos `mod` wrapAt
          newCount = count + countZeros pos newPos wrapAt

countZeros :: Int -> Int -> Int -> Int
countZeros prev curr wrapAt
    | (prev < 0 || prev >= wrapAt) = error "Position out of bounds"
    | (prev == 0 && curr < 0) = abs(curr `div` wrapAt) - 1
    | (curr == 0) = abs(curr `div` wrapAt) + 1
    | (curr `mod` wrapAt) == 0 = abs(curr `div` wrapAt)
    | otherwise = abs(curr `div` wrapAt)
