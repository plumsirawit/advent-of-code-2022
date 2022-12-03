import System.Environment

updateMax (mx, smx, tmx) cur
  | cur >= mx = (cur, mx, smx)
  | cur >= smx = (mx, cur, smx)
  | cur >= tmx = (mx, smx, cur)
  | otherwise = (mx, smx, tmx)

helper :: Int -> (Int, Int, Int) -> [String] -> (Int, Int, Int)
helper sm topTup lines
  | null lines = topTup
  | null (head lines) = helper 0 (updateMax topTup sm) (tail lines)
  | otherwise = helper (sm + read (head lines) :: Int) topTup (tail lines)

main = do
  args <- getArgs
  let fname = head args
  content <- readFile fname
  let (mx, smx, tmx) = helper 0 (-1, -1, -1) (lines content)
  print (mx + smx + tmx)
