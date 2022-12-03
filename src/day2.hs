import System.Environment

theirScore theirMove
  | theirMove == 'A' = 1
  | theirMove == 'B' = 2
  | theirMove == 'C' = 3
  | otherwise = 0

myScore myMove
  | myMove == 'X' = 1
  | myMove == 'Y' = 2
  | myMove == 'Z' = 3
  | otherwise = 0

winScore theirMove myMove
  | theirMove == 'A' && myMove == 'Z' = 0
  | theirMove == 'C' && myMove == 'X' = 6
  | theirScore theirMove == myScore myMove = 3
  | myScore myMove > theirScore theirMove = 6
  | otherwise = 0

oneRound theirMove myMove = myScore myMove + winScore theirMove myMove

getMyMove theirMove myResult
  | winScore theirMove 'X' == (myScore myResult - 1) * 3 = 'X'
  | winScore theirMove 'Y' == (myScore myResult - 1) * 3 = 'Y'
  | otherwise = 'Z'

-- try all the possible ways
newOneRound theirMove myResult = oneRound theirMove (getMyMove theirMove myResult)

helper inputLines
  | null inputLines = 0
  | otherwise = newOneRound (head inputLines !! 0) (head inputLines !! 2) + helper (tail inputLines)

main = do
  args <- getArgs
  let fname = head args
  content <- readFile fname
  print (helper (lines content))
