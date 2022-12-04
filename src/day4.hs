import Data.Char
import Data.Foldable (Foldable (toList))
import Data.Sequence (fromList, mapWithIndex)
import System.Environment

enumerate :: String -> [(Int, Char)]
enumerate st = toList (mapWithIndex (,) (fromList st))

findChar :: Char -> String -> Int
findChar ch st = fst (head (filter (\p -> snd p == ch) (enumerate st)))

splitAtChar ch st = let (p, q) = splitAt (findChar ch st) st in (p, tail q)

splitToTwoPairs = splitAtChar ','

splitToTwoInts :: String -> (Int, Int)
splitToTwoInts st = let (p, q) = splitAtChar '-' st in (read p, read q)

lineToIntervals ln = let (p, q) = splitToTwoPairs ln in (splitToTwoInts p, splitToTwoInts q)

containsInterval :: ((Int, Int), (Int, Int)) -> Int
containsInterval ((x1, y1), (x2, y2))
  | x1 <= x2 && x2 <= y2 && y2 <= y1 = 1
  | x2 <= x1 && x1 <= y1 && y1 <= y2 = 1
  | otherwise = 0

intersectInterval ((x1, y1), (x2, y2)) = (max x1 x2, min y1 y2)

checkEmpty (a, b)
  | a <= b = 1
  | otherwise = 0

helper lines
  | null lines = 0
  | otherwise = containsInterval (lineToIntervals (head lines)) + helper (tail lines)

helper2 lines
  | null lines = 0
  | otherwise = checkEmpty (intersectInterval (lineToIntervals (head lines))) + helper2 (tail lines)

main = do
  args <- getArgs
  let fname = head args
  content <- readFile fname
  print (helper2 (lines content))
