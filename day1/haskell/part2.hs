-- 6430 too high
-- 6300 too low

main :: IO ()
main = do
    input <- getContents
    let twists = map parseLine $ lines input
    let states = foldl applyMoveCountZeros (50, 0) twists
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
          newCount = count + countZeros pos delta wrapAt

countZeros :: Int -> Int -> Int -> Int
countZeros pos delta wrapAt = 
    let range = tail [pos, pos + signum delta .. pos + delta]
    in length [x | x <- range, x `mod` wrapAt == 0]
