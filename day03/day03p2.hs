import Data.Char
import Data.Maybe
import qualified Data.Set as Set

data Part = Part {numId :: Int, value:: Int}
  deriving (Ord, Eq)

data Tile = PartTile Part | Gear | Blank

appendDigit :: Int -> Char -> Int
appendDigit n c = (n*10) + (digitToInt c)

isGearChar :: Char -> Bool
isGearChar = ('*' ==)

lineToTiles' :: String -> Int -> Int -> [Tile]
lineToTiles' [] _ _ = []

lineToTiles' [c] numId soFar
  | isDigit c = [PartTile (Part numId (appendDigit soFar c))]
  | soFar == 0 && isGearChar c = [Gear]
  | soFar == 0 = [Blank]
  | otherwise = [PartTile (Part numId soFar)]

lineToTiles' (c1:c2:cs) numId soFar
  | isDigit c1 && isDigit c2 = (head nextNums):nextNums
  | currentNum == 0 = (if isGearChar c1 then Gear else Blank):(lineToTiles' (c2:cs) numId 0)
  | otherwise = PartTile (Part numId currentNum):lineToTiles' (c2:cs) (numId+1) 0
  where
    currentNum = if isDigit c1 then appendDigit soFar c1 else soFar
    nextNums = lineToTiles' (c2:cs) numId currentNum

lineToTiles :: String -> Int -> [Tile]
lineToTiles line numId = lineToTiles' line numId 0

maxId :: [Tile] -> Maybe Int
maxId [] = Nothing
maxId (Gear:xs) = maxId xs
maxId (Blank:xs) = maxId xs
maxId ((PartTile (Part numId _)):xs) = Just (maybe numId (max numId) (maxId xs))

linesToTiles' :: [String] -> Int -> [[Tile]]
linesToTiles' [] _ = []
linesToTiles' (line:rest) numId =
  tileLine:linesToTiles' rest (maybe numId (1+) (maxId tileLine))
  where tileLine = lineToTiles line numId

linesToTiles :: [String] -> [[Tile]]
linesToTiles lines = linesToTiles' lines 1

enumerate :: [Tile] -> [(Int, Tile)]
enumerate = zip [0..]

isGear :: Tile -> Bool
isGear Gear = True
isGear _ = False

sslice :: Int -> Int -> String -> String
sslice low high tiles =
  drop (max low 0) (take (min high (length tiles)) tiles) 

slice :: Int -> Int -> [Tile] -> [Tile]
slice low high tiles =
  drop (max low 0) (take (min high (length tiles)) tiles) 

findParts' :: [Tile] -> [Part]
findParts' [] = []
findParts' (PartTile p:ts) = p:findParts' ts
findParts' (_:ts) = findParts' ts

findParts :: [Tile] -> Set.Set Part
findParts = Set.fromList . findParts'

adjacentParts :: [[Tile]] -> Int -> Set.Set Part
adjacentParts rows index =
  foldl Set.union Set.empty (map (findParts . slice (index-1) (index+2)) rows)

gearRatio' :: [Part] -> Int
gearRatio' [Part _ v1,Part _ v2] = v1 * v2
gearRatio' _ = 0

gearRatio :: Set.Set Part -> Int
gearRatio = gearRatio' . Set.toList

countParts :: [Tile] -> [Tile] -> [[Tile]] -> Int
countParts prev current [] = 0
countParts prev current (next:rest) =
  (countParts current next rest) + (
  sum $ map (gearRatio . adjacentParts [prev, current, next] . fst)  $ filter (isGear . snd) (enumerate current))

countPartsTotal :: [[Tile]] -> Int
countPartsTotal (ln1:ln2:lns) = countParts ln1 ln2 lns

main :: IO ()
main = do
  input <- getContents
  let parsedLines = linesToTiles (lines input)
      emptyLine = replicate (length (parsedLines!!0)) Blank
      allLines = emptyLine:parsedLines ++ [emptyLine]
    in print $ countPartsTotal allLines
