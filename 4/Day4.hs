import System.IO
import Data.Tuple

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print $ solvePart1 contents
    hClose handle

rangeContained :: (Ord a) => ((a,a),(a,a)) -> Bool
rangeContained a = (snd . snd $ a) <= (snd . fst $ a)

orderFirstSame :: (Ord a) => ((a,a),(a,a)) -> ((a,a),(a,a))
orderFirstSame a
    | (first a) <= (second a) = swap a
    | otherwise = a
    where first = snd . fst
          second = snd . snd

orderTuples :: (Ord a) => ((a,a), (a,a)) -> ((a,a), (a,a)) 
orderTuples a
    | (first a) == (second a) = orderFirstSame a
    | (first a) <= (second a) = a
    | otherwise = swap a
    where first = fst . fst
          second = fst . snd

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple f (x,y) = (f x, f y)

extractRange :: String -> (Int, Int)
extractRange =  mapTuple read . splitOn '-'

splitOn :: Char -> String -> (String, String)
splitOn c = removeDelimiter . break (== c)
    where removeDelimiter (s1, s2) = (s1, drop 1 s2) 

solvePart1 :: String -> Int
solvePart1 = length . filter rangeContained . map (orderTuples . mapTuple extractRange . splitOn ','). lines

