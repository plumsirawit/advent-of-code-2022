import Data.Char
import Data.Foldable (Foldable (toList))
import Data.Sequence (fromList, mapWithIndex)
import System.Environment

sliceFourFront st = fst (splitAt 14 st)
orArr arr = foldr (\x y -> x || y) False arr
checkDistinct st
  | null st = True
  | otherwise = let c = (head st) in (not (orArr (map (== c) (tail st)))) && checkDistinct (tail st) 

helper line i
  | length line == 14 = i
  | otherwise = if checkDistinct (sliceFourFront line) then i else helper (tail line) (i+1)

main = do
  args <- getArgs
  let fname = head args
  content <- readFile fname
  print (14 + helper (head (lines content)) 0)
