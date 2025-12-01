main :: IO ()
main = do
    input <- getContents
    let positions = scanl applyMove 50 (map parseLine $ lines input)
    print $ length $ filter (== 0) positions

parseLine :: String -> Int
parseLine ('L':rest) = -(read rest)
parseLine ('R':rest) = read rest
parseLine _          = error "Invalid input"

applyMove :: Int -> Int -> Int
applyMove pos delta = (pos + delta + 100) `mod` 100