main :: IO ()
main = do
    input <- getContents
    let banks = parseLines input
    let maxJoltages = map findMaxJoltage banks
    let totalJoltage = sum maxJoltages
    print totalJoltage

parseLine :: String -> [Int]
parseLine l = map (\c -> read [c]) l

parseLines :: String -> [[Int]]
parseLines i = map parseLine (lines i)

getBankAfter :: Int -> [Int] -> [Int]
getBankAfter battery [] = error "Bank should not be empty"
getBankAfter battery (firstBattery:remaining)
    | firstBattery == battery = remaining
    | otherwise = getBankAfter battery remaining

findMaxJoltage :: [Int] -> Int
findMaxJoltage bank = firstBattery * 10 + secondBattery
    where firstBattery = maximum $ init bank
          secondBattery = maximum $ getBankAfter firstBattery bank
