import Data.Bits (shiftL)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

splitCard :: String -> [[Int]]
splitCard = map (map (read :: String -> Int)) . (splitOn ["|"]) . words . (drop 1) . (dropWhile (/= ':'))

scoreCard :: [[Int]] -> Int
scoreCard [ns1, ns2]
  | numMatches == 0 = 0
  | otherwise = 1 `shiftL` (numMatches - 1)
  where
    numMatches = length (Set.intersection (Set.fromList ns1) (Set.fromList ns2))

main :: IO ()
main = do
  input <- getContents
  print $ sum (map (scoreCard . splitCard) (lines input))

