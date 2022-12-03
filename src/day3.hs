import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment

convertCharToPriority :: Char -> Int
convertCharToPriority x
  | ord x < 97 = ord x - 65 + 27
  | otherwise = ord x - 97 + 1

updateHistogram :: Map.Map Int Int -> Char -> Map.Map Int Int
updateHistogram hist c = Map.insert (convertCharToPriority c) 1 hist

makeFullHistogram = foldl updateHistogram Map.empty

makeLeftHistogram rucksack = makeFullHistogram (take (div (length rucksack) 2) rucksack)

makeRightHistogram rucksack = makeFullHistogram (drop (div (length rucksack) 2) rucksack)

auxAccum :: Int -> Int -> Int -> Int
auxAccum prior current culprit = culprit + prior * current

sumKeys = Map.foldrWithKey auxAccum 0

solveOneRucksack :: String -> Int
solveOneRucksack rucksack = sumKeys (Map.intersection (makeLeftHistogram rucksack) (makeRightHistogram rucksack))

helper :: [String] -> Int
helper inputLines
  | null inputLines = 0
  | otherwise = solveOneRucksack (head inputLines) + helper (tail inputLines)

solveGroup r1 r2 r3 = sumKeys (Map.intersection (Map.intersection (makeFullHistogram r1) (makeFullHistogram r2)) (makeFullHistogram r3))

helper2 :: [String] -> Int
helper2 inputLines
  | null inputLines = 0
  | otherwise = solveGroup (head inputLines) (head (tail inputLines)) (head (tail (tail inputLines))) + helper2 (tail (tail (tail inputLines)))

main = do
  args <- getArgs
  let fname = head args
  content <- readFile fname
  print (helper2 (lines content))
