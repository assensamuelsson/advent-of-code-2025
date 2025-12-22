import Data.List.Split (splitOn)

data Range = Range {
    start :: Integer,
    end :: Integer
} deriving (Show)

parseInput :: String -> ([Range], [Integer])
parseInput input = (ranges, ingredients)
  where
    [rangesSection, ingredientsSection] = splitOn "\n\n" $ input
    ranges = map parseRange $ lines rangesSection
    ingredients = map read $ lines ingredientsSection
    
    parseRange line = let [a, b] = splitOn "-" line
                      in Range {start=(read a), end=(read b)}

isInRange :: Integer -> Range -> Bool
isInRange ingredient (Range start end) = ingredient >= start && ingredient <= end

isInAnyRange :: [Range] -> Integer -> Bool
isInAnyRange ranges ingredient = any (isInRange ingredient) ranges

main :: IO ()
main = do
    input <- getContents
    let (ranges, ingredients) = parseInput input
    let fresh = filter (isInAnyRange ranges) ingredients
    print (length fresh)
