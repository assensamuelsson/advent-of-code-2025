import Data.List.Split (splitOn)

data Range = Range {
    start :: Integer,
    end :: Integer
} deriving (Show)

main :: IO ()
main = do
    input <- getContents
    let (ranges, _) = parseInput input
    let combined = combineAllRanges ranges
    let total = sum $ map (\(Range s e) -> e - s + 1) combined
    print total

parseInput :: String -> ([Range], [Integer])
parseInput input = (ranges, ingredients)
  where
    [rangesSection, ingredientsSection] = splitOn "\n\n" $ input
    ranges = map parseRange $ lines rangesSection
    ingredients = map read $ lines ingredientsSection
    
    parseRange line = let [a, b] = splitOn "-" line
                      in Range {start=(read a), end=(read b)}

combineTwoRanges :: Range -> Range -> [Range]
combineTwoRanges (Range s1 e1) (Range s2 e2)
    | s1 <= e2 && e1 >= s2 = [Range {start=(min s1 s2), end=(max e1 e2)}]
    | otherwise = [Range {start=s1, end=e1}, Range {start=s2, end=e2}]

combineRangeIntoRanges :: Range -> [Range] -> [Range]
combineRangeIntoRanges range [] = [range]
combineRangeIntoRanges range (r:rs) = 
    case combineTwoRanges range r of
        [combined] -> combineRangeIntoRanges combined rs
        [_, _] -> r : combineRangeIntoRanges range rs

combineAllRanges :: [Range] -> [Range]
combineAllRanges = foldr combineRangeIntoRanges []
