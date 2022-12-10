import System.IO
import Data.Char
import qualified Data.Set as Set

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print $ solvePart1 contents
    print $ solvePart2 contents
    hClose handle

inRange :: Char -> Char -> Char -> Bool
inRange c r1 r2 = (ord c) >= (ord r1) && (ord c) <= (ord r2)

priority :: Char -> Int
priority c
  | inRange c 'A' 'Z' = (ord c) - 38
  | inRange c 'a' 'z' = (ord c) - 96

findDuplicate :: ([Char], [Char])-> [Char]
findDuplicate ([], _) = []
findDuplicate ((head:tail), c) = 
    case elem head c of
        False -> findDuplicate (tail, c)
        True -> head : findDuplicate (tail, c)

groupN :: Int -> [a] -> [[a]]
groupN _ []= []
groupN n x = (take n x) : (groupN n (drop n x)) 

findBadge :: [String] -> Char
findBadge [(head:tail),s2,s3] = 
    let elemInAll c = (elem c s2) && (elem c s3) in
    case elemInAll head of
        True -> head
        False -> findBadge [tail, s2, s3]
    

solvePart1 :: String -> Int
solvePart1 = sum . map (sum . map priority . removeDuplicates . findDuplicate . splitCompartments) . lines
    where splitCompartments r = splitAt ((length r) `div` 2) r  
          removeDuplicates = Set.toList . Set.fromList

solvePart2 :: String -> Int
solvePart2 = sum . map (priority . findBadge) . groupN 3 . lines
