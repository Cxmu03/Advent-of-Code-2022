import System.IO
import Text.Read
import Control.Monad
import Data.Maybe
import Data.List

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print $ solvePart1 contents
    print $ solvePart2 contents
    hClose handle

sumPure :: (Monad f, Num a) => [f a] -> f a
sumPure = foldM (fmap . (+)) 0

group :: [Maybe Int] -> [[Maybe Int]]
group s = 
    let cleanedList = dropWhile isNothing s
        (newHead, newTail) = break isNothing cleanedList in
    case cleanedList of
    [] -> []
    _ -> newHead : (Main.group newTail)


sumElves :: String -> [Maybe Int]
sumElves = map sumPure . Main.group . numbers . lines
    where numbers = map (readMaybe)

solvePart1 :: String -> Maybe Int
solvePart1 = maximum . sumElves

solvePart2 :: String -> Maybe Int
solvePart2 = sumPure . take 3 . sortBy descending . sumElves
    where descending = (flip compare)
