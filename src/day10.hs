import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment

processLine line x output numCycle
  | head line == "noop" = (x, (if abs (x - mod numCycle 40) <= 1 then 1 else 0) : output, numCycle+1)
  | head line == "addx" = processLine ("addx2" : tail line) x ((if abs (x - mod numCycle 40) <= 1 then 1 else 0) : output) (numCycle+1)
  | head line == "addx2" = let nx = x + read (head (tail line)) in (nx, (if abs (x - mod numCycle 40) <= 1 then 1 else 0) : output, numCycle+1)

helper :: [String] -> Int -> [Int] -> Int -> [Int]
helper lines x output numCycle
  | null lines = output
  | otherwise = let (nx, noutput, nnumCycle) = processLine (words (head lines)) x output numCycle in helper (tail lines) nx noutput nnumCycle 

convert ch
  | ch == 0 = '.'
  | otherwise = '#'

parseOutput :: [Int] -> [String] -> [String]
parseOutput [] buffer = buffer
parseOutput t buffer = foldl
      (\ buffer h
         -> if null buffer || length (head buffer) == 40 then
                [convert h] : buffer
            else
                (convert h : head buffer) : tail buffer)
      buffer t

-- = let nbuffer = if length (head buffer) == 40 then [convert h] : buffer else (convert h : tail (head buffer)) : tail buffer in parseOutput t nbuffer

main = do
  args <- getArgs
  let fname = head args
  content <- readFile fname
  mapM print (parseOutput (helper (lines content) 1 [] 0) [])