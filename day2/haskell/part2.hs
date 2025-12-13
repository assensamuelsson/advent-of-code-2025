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

-- Split into k equal parts if length is divisible by k
splitEqualParts :: Int -> [a] -> Maybe [[a]]
splitEqualParts k xs
  | k <= 0             = Nothing
  | len `mod` k /= 0   = Nothing
  | otherwise          = Just (chunksOf n xs)
  where
    len = length xs
    n   = len `div` k

-- All equal splits for every k that divides the length (k from 2..len)
allEqualParts :: [a] -> [[[a]]]
allEqualParts xs =
  [ chunksOf (len `div` k) xs
  | let len = length xs
  , k <- [2..len]
  , len `mod` k == 0
  ]

-- Helper: chunk list into fixed-size pieces (assumes n > 0 and len xs multiple of n)
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (h, t) = splitAt n xs
  in h : chunksOf n t

-- Are all elements equal?
allSame :: Eq a => [a] -> Bool
allSame []     = False
allSame (x:xs) = all (== x) xs

-- True if the string can be split into k>=2 equal parts that are identical
hasEqualSplit :: Integer -> Bool
hasEqualSplit n =
  let s   = show (abs n)
      len = length s
      try k = case splitEqualParts k s of
                Just parts -> allSame parts
                Nothing    -> False
  in any try [2 .. len]

findDuplicates :: (Integer, Integer) -> [Integer]
findDuplicates (start, end) = [x | x <- [start..end], hasEqualSplit x]
