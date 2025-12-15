main :: IO ()
main = do
    input <- getContents
    let banks = parseLines input
    let maxJoltages = map (findMaxJoltage []) banks
    let totalJoltages = map (foldl (\acc b -> acc * 10 + b) 0) maxJoltages
    let totalJoltage = sum totalJoltages
    print totalJoltage

parseLine :: String -> [Int]
parseLine l = map (\c -> read [c]) l

parseLines :: String -> [[Int]]
parseLines i = map parseLine (lines i)

getBankAfter :: Int -> [Int] -> [Int]
getBankAfter _ [] = error "Bank should not be empty"
getBankAfter battery (firstBattery:remaining)
    | firstBattery == battery = remaining
    | otherwise               = getBankAfter battery remaining

findMaxJoltage :: [Int] -> [Int] -> [Int]
findMaxJoltage chosen bank
    | chosenCount == batteriesToTurnOn = chosen
    | length bank < need               = error "Not enough remaining batteries"
    | otherwise                        = findMaxJoltage (chosen ++ [battery]) (getBankAfter battery bank)
    where 
        batteriesToTurnOn = 12
        chosenCount = length chosen
        need = batteriesToTurnOn - chosenCount
        allowedRaw = length bank - need + 1
        allowed = max 1 allowedRaw
        battery = maximum $ take allowed bank
