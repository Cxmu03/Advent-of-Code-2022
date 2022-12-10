import qualified Data.Map as Map
import System.IO
import Control.Monad
import Control.Arrow

scoreMap :: Map.Map String Int
scoreMap = Map.fromList [("X", 0), ("Y", 1), ("Z", 2)]

enemyValueMap :: Map.Map String Int
enemyValueMap = Map.fromList [("A", 0), ("B", 1), ("C", 2)]

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print $ solvePart1 contents
    hClose handle

roundResult :: String -> String -> Int
roundResult e p
  | (eVal == pVal) = 3
  | (((`mod` 3) <$> ((+1) <$> eVal)) == pVal) = 6
  | otherwise = 0
    where
        eVal = Map.lookup e enemyValueMap
        pVal = Map.lookup p scoreMap

round :: [String] -> Maybe Int
round [] = Nothing
round (x:y:[]) = (fmap . (+)) result pickValue
    where pickValue =  (+) <$> (Just 1) <*> Map.lookup y scoreMap
          result = roundResult x y

solvePart1 :: String -> Maybe Int
solvePart1 = foldM (fmap . (+)) 0 . map Main.round . map words . lines
