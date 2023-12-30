maxCubes :: [String] -> Char -> Int
maxCubes [] _ = 0
maxCubes (n:c:ncs) matchColor =
  if matchColor == color
  then max num restMax
  else restMax
  where
    restMax = maxCubes ncs matchColor
    num = (read n) :: Int
    color = c!!0

power :: [String] -> Int
power ncs = product (map (maxCubes ncs) "rgb")

main :: IO ()
main = do
  input <- getContents
  print $ sum (map (power . drop 2 . words) (lines input))
