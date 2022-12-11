import Data.Char
import System.Environment

checkToTop table value row col
  | row < 0 = 0
  | otherwise = if value <= ((table !! row) !! col) then row else checkToTop table value (row-1) col

checkToRight table value row col
  | col >= length (head table) = (length (head table)) - 1
  | otherwise = if value <= ((table !! row) !! col) then col else checkToRight table value row (col+1)

checkToLeft table value row col
  | col < 0 = 0
  | otherwise = if value <= ((table !! row) !! col) then col else checkToLeft table value row (col-1)

checkToBottom table value row col
  | row >= length table = length table - 1
  | otherwise = if value <= ((table !! row) !! col) then row else checkToBottom table value (row+1) col

scenicScore table row col = let value = (table !! row) !! col in (row - checkToTop table value (row-1) col) * ((checkToRight table value row (col+1)) - col) * (col - checkToLeft table value row (col-1)) * ((checkToBottom table value (row+1) col) - row)

helper table num cnt
  | num == -1 = cnt
  | otherwise = max (helper table (num-1) cnt) (scenicScore table (div num (length (head table))) (mod num (length (head table))))

check :: [[Int]] -> Int -> IO (Int)
check table idx = if idx == -1 then pure 0 else do
  let row = div idx (length (head table))
  let col = mod idx (length (head table))
  print ((table !! row) !! col - 48)
  check table (idx-1)

main = do
  args <- getArgs
  let fname = head args
  content <- readFile fname
  let table = map (\st -> (map ord st)) (lines content)
  -- check table (length table * length (head table) - 1)
  print (helper table (length table * length (head table) - 1) 0)