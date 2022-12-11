import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment

contractLine h t
  | h < t = t -1
  | h > t = t + 1
  | otherwise = t

contractSingle (hx, hy) (tx, ty) = if (abs (hx - tx) > 1) || (abs (hy - ty) > 1) then (contractLine hx tx, contractLine hy ty) else (tx, ty)

contractRope :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
contractRope coordH coordT
  | null coordT = coordT
  | otherwise = let contracted = contractSingle coordH (head coordT) in contracted : contractRope contracted (tail coordT)

updateVisitedSet :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
updateVisitedSet visitedSet (tx, ty) = if Set.member (tx, ty) visitedSet then visitedSet else Set.insert (tx, ty) visitedSet

moveDown :: Int -> [(Int, Int)] -> Set (Int, Int) -> ([(Int, Int)], Set (Int, Int))
moveDown 0 coordList visitedSet = (coordList, visitedSet)
moveDown num ((hx, hy) : coordT) visitedSet = let newCoordT = contractRope (hx + 1, hy) coordT in moveDown (num -1) ((hx + 1, hy) : newCoordT) (updateVisitedSet visitedSet (newCoordT !! 8))
moveDown num _ visitedSet = error "empty coord"

moveLeft :: Int -> [(Int, Int)] -> Set (Int, Int) -> ([(Int, Int)], Set (Int, Int))
moveLeft 0 coordList visitedSet = (coordList, visitedSet)
moveLeft num ((hx, hy) : coordT) visitedSet = let newCoordT = contractRope (hx, hy -1) coordT in moveLeft (num -1) ((hx, hy -1) : newCoordT) (updateVisitedSet visitedSet (newCoordT !! 8))
moveLeft num _ visitedSet = error "empty coord"

moveRight :: Int -> [(Int, Int)] -> Set (Int, Int) -> ([(Int, Int)], Set (Int, Int))
moveRight 0 coordList visitedSet = (coordList, visitedSet)
moveRight num ((hx, hy) : coordT) visitedSet = let newCoordT = contractRope (hx, hy + 1) coordT in moveRight (num -1) ((hx, hy + 1) : newCoordT) (updateVisitedSet visitedSet (newCoordT !! 8))
moveRight num _ visitedSet = error "empty coord"

moveUp :: Int -> [(Int, Int)] -> Set (Int, Int) -> ([(Int, Int)], Set (Int, Int))
moveUp 0 coordList visitedSet = (coordList, visitedSet)
moveUp num ((hx, hy) : coordT) visitedSet = let newCoordT = contractRope (hx -1, hy) coordT in moveUp (num -1) ((hx -1, hy) : newCoordT) (updateVisitedSet visitedSet (newCoordT !! 8))
moveUp num _ visitedSet = error "empty coord"

moveOneLine coordList command visitedSet = case head (head (words command)) of
  'D' -> moveDown (read (head (tail (words command)))) coordList visitedSet
  'L' -> moveLeft (read (head (tail (words command)))) coordList visitedSet
  'U' -> moveUp (read (head (tail (words command)))) coordList visitedSet
  'R' -> moveRight (read (head (tail (words command)))) coordList visitedSet
  _ -> (coordList, visitedSet)

helper lines coordList visitedSet
  | null lines = visitedSet
  | otherwise = let (newCoordList, newVisitedSet) = moveOneLine coordList (head lines) visitedSet in helper (tail lines) newCoordList newVisitedSet

main = do
  args <- getArgs
  let fname = head args
  content <- readFile fname
  print (length (helper (lines content) (replicate 10 (0, 0)) (Set.fromList [(0, 0)])))