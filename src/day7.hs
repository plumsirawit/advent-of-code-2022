import Data.Char
import System.Environment

data FileTree = Branch String Int [FileTree]

findInTree newDir (Branch dirName fileSize children) = foldl (\old (Branch cur a b) -> if cur == newDir then Branch cur a b else old) (Branch "" 0 []) children

convert (Branch oldDirName oldFileSize oldChildren) (Branch newDirName newFileSize newChildren) = if oldDirName == newDirName then Branch newDirName newFileSize newChildren else Branch oldDirName oldFileSize oldChildren

replaceInTree (Branch newDirName newFileSize newChildren) (Branch dirName fileSize children) = Branch dirName fileSize (map (\node -> convert node (Branch newDirName newFileSize newChildren)) children)

prop parList
  | length parList < 2 = parList
  | otherwise = let (leaf, par, rest) = (head parList, head (tail parList), tail (tail parList)) in (replaceInTree leaf par : rest)

convToChildren (Branch a b c) = foldl (\s t -> s ++ " [" ++ t ++ "]") "" (map (\(Branch x y z) -> x) c)

-- head parList is the current node
changeDirectory newDir parList
  | newDir == ".." = prop parList
  | newDir == "/" && length parList >= 2 = let propagatedParList = prop parList in changeDirectory "/" propagatedParList
  | newDir == "/" && length parList < 2 = parList
  | otherwise = let (Branch a b c) = findInTree newDir (head parList) in if a == "" then error ("not found " ++ newDir ++ " in" ++ convToChildren (head parList)) else Branch a b c : parList

processCommand args parList
  | head args == "cd" = changeDirectory (head (tail args)) parList
  | head args == "ls" = parList
  | otherwise = error "command not recognized"

addChildToNode (Branch dirName fileSize children) node = Branch dirName fileSize (node : children)

createDir dirName = Branch dirName 0 []

createFile fileSize fileName = Branch fileName fileSize []

processLine line parList
  | head line == '$' = processCommand (tail (words line)) parList
  | head line == 'd' = let newCurrentNode = addChildToNode (head parList) (createDir (head (tail (words line)))) in (newCurrentNode : tail parList)
  | otherwise = let newCurrentNode = addChildToNode (head parList) (createFile (read (head (words line))) (head (tail (words line)))) in (newCurrentNode : tail parList)

addIOInt :: IO Int -> IO Int -> IO Int
addIOInt a b = do
  aa <- a
  bb <- b
  return (aa + bb)

propSize :: FileTree -> FileTree
propSize (Branch dirName fileSize children) = let newNodes = map propSize children in Branch dirName (fileSize + sum (map (\(Branch a b c) -> b) newNodes)) newNodes

helper :: [String] -> [FileTree] -> FileTree
helper lines parList
  | null lines = head (changeDirectory "/" parList)
  | otherwise = helper (tail lines) (processLine (head lines) parList)

sumSmallFiles (Branch a b c) = sum (map sumSmallFiles c) + (if not (null c) && b < 100000 then b else 0)

deleteHelper :: Int -> FileTree -> Int -> Int
deleteHelper lowerBound (Branch a b c) cur = foldl (\old mn -> if (mn >= lowerBound) && (mn < old) then mn else old) cur ((if not (null c) then b else 1000000000) : map (\x -> deleteHelper lowerBound x cur) c)

main = do
  args <- getArgs
  let fname = head args
  content <- readFile fname
  let root = propSize (helper (lines content) [Branch "root" 0 []])
  let Branch a b c = root
  print (sumSmallFiles root)
  print (deleteHelper (b - 40000000) root 1000000000)
