import Data.List.Split (splitOn)

main :: IO ()
main = do
    input <- getContents
    let ranges = parseLine input
    let result = sum $ concatMap findDuplicates ranges
    print result

parseLine :: String -> [(Integer, Integer)]
parseLine line = map toRange (splitOn "," line)
    where toRange r = let [start, end] = splitOn "-" r
                      in (read start, read end)

splitMiddle :: String -> (String, String)
splitMiddle s = splitAt (length s `div` 2) s

equalHalves :: Integer -> Bool
equalHalves n =
    let s = show n
        (a, b) = splitMiddle s
    in even (length s) && a == b

findDuplicates :: (Integer, Integer) -> [Integer]
findDuplicates (start, end) = [x | x <- [start..end],  equalHalves x]
