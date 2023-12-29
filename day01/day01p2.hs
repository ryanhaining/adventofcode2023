import Data.Char
import Data.List

checkForNumber :: (String -> String) -> String -> Int
checkForNumber prefixMod (c:cs) 
  | isDigit c = digitToInt c
  | isPrefixOf (prefixMod "one") str = 1
  | isPrefixOf (prefixMod "two") str = 2
  | isPrefixOf (prefixMod "three") str = 3
  | isPrefixOf (prefixMod "four") str = 4
  | isPrefixOf (prefixMod "five") str = 5
  | isPrefixOf (prefixMod "six") str = 6
  | isPrefixOf (prefixMod "seven") str = 7
  | isPrefixOf (prefixMod "eight") str = 8
  | isPrefixOf (prefixMod "nine") str = 9
  | otherwise = checkForNumber prefixMod cs
  where str = c:cs

firstNumber :: String -> Int
firstNumber = checkForNumber id

-- reverse the input string and reverse the number strings
lastNumber :: String -> Int
lastNumber = checkForNumber reverse . reverse

calibrationValue :: String -> Int
calibrationValue cs = (firstNumber cs) * 10 + (lastNumber cs)

main :: IO ()
main = do
  inp <- getContents
  print $ sum (map calibrationValue (lines inp))
