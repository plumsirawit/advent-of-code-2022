import Data.Char
import Data.Foldable (Foldable (toList))
import Data.Sequence (fromList, mapWithIndex)
import System.Environment

retrieveOne from stk = head (stk !! from)
removeOne from stk = let (p, q) = splitAt from stk in p ++ (tail (head q)) : (tail q)
pushTo num to stk = let (p, q) = splitAt to stk in p ++ (num : (head q)) : (tail q)
moveOne from to stk = pushTo (retrieveOne from stk) to (removeOne from stk)

reverseFront num st = let (p, q) = splitAt num st in (reverse p) ++ q
reverseTopStack num to stk = let (p, q) = splitAt to stk in p ++ (reverseFront num (head q)) : (tail q)

moveStack num from to stk
  | num == 0 = stk
  | otherwise = moveStack (num-1) from to (moveOne from to stk)

wrapMoveStack num from to stk = reverseTopStack num to (moveStack num from to stk)

parseOperation line stk = wrapMoveStack (read ((words line) !! 1)) (read ((words line) !! 3) - 1) (read ((words line) !! 5) - 1) stk

addToStack line stk = map (\(p, q) -> if p == ' ' then q else q ++ [p]) (zip (map (\i -> line !! i) [1,5..33]) stk)

helper lines stk ready
  | null lines = map head (map (\x -> x ++ "$") stk)
  | not ready && not (null (head lines)) = helper (tail lines) (addToStack (head lines) stk) ready
  | null (words (head lines)) = helper (tail lines) stk True
  | otherwise = helper (tail lines) (parseOperation (head lines) stk) ready

main = do
  args <- getArgs
  let fname = head args
  content <- readFile fname
  print (helper (lines content) (replicate 9 "") False)
