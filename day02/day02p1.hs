gameNumber :: [String] -> Int
gameNumber (_:n:_) = read (init n) :: Int

isValidGame' :: [String] -> Bool
isValidGame' [] = True
isValidGame' (n:c:ncs) =
  case color of
    'r' -> num <= 12 && isValidGame' ncs
    'g' -> num <= 13 && isValidGame' ncs
    'b' -> num <= 14 && isValidGame' ncs
  where
    color = c!!0
    num = (read n) :: Int

isValidGame :: [String] -> Bool
isValidGame = isValidGame' . drop 2

main :: IO ()
main = do
  input <- getContents
  print $ sum (map gameNumber (filter isValidGame (map words (lines input))))
