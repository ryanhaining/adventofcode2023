import Data.Char

firstDigit :: String -> Int
firstDigit (c:cs) = if (isDigit c) then (digitToInt c) else firstDigit cs


calibrationValue :: String -> Int
calibrationValue cs = (firstDigit cs) * 10 + (firstDigit (reverse cs))

main :: IO ()
main = do
  inp <- getContents
  print $ sum (map calibrationValue (lines inp))
