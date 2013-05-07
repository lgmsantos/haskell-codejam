import CodeJam

import Data.List
import Data.Maybe

type Key = Int
type Chest = (Int, Key, [Key])
data ProblemD = D [Key] [Chest] | Impossible deriving Show

chestIndex (i, _, _) = i
keyToOpen (_, k, _) = k
keysInside (_, _, ks) = ks

getKeys (D keys _) = keys
getChests (D _ chests) = chests
requiredKeys (D _ chests) = map keyToOpen chests
availableKeys (D keys chests) = concat $ keys : map keysInside chests

keyDeadlock p@(D keys chests) = elem 1 . map keyCount $ keys'
    where keys' = map keyToOpen . filter selfOpen $ chests
          selfOpen (_, k, ks) = elem k ks
          keyCount k = length . filter (== k) . availableKeys $  p

instance Problem ProblemD where
    readProblem = do
        (keyCount, chestCount) <- consumePair
        keys <- consume keyCount
        chests <- sequence $ map consumeChest [1..chestCount]
        return (D (map read keys) chests)
        where consumeChest i = do
                (k:_) <- consume 1
                ks <- consumeN
                return (i, read k, map read ks)

    showSolution = solve 

solve problem = case solve' problem [] of
                    Nothing -> "IMPOSSIBLE"
                    Just xs -> concat . intersperse " " . map show $ xs

solve' Impossible _ = Nothing
solve' (D _ []) visited = Just . reverse . map chestIndex $ visited
solve' (D [] (_:_)) _ = Nothing
solve' problem visited 
    | not . null $ requiredKeys problem \\ availableKeys problem = Nothing 
    | keyDeadlock problem = Nothing
    | null solutions = Nothing
    | otherwise = head solutions
    where solutions = filter isJust . map open' . getChests $ problem
          open' c = solve' (open problem c) (c:visited)

open (D keys chests) c@(i, k, ks) 
    | elem k keys = D (delete k keys ++ ks) (delete c chests)
    | otherwise = Impossible

main = mainCodeJam (loadProblems :: FilePath -> IO [ProblemD])