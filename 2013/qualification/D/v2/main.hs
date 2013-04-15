import CodeJam

import Data.List
import Data.Function
import Data.Maybe
import Control.Monad.State
import Debug.Trace

type Key = Int 
type Chest = (Int, Key, [Key])
data ProblemD = D [Key] [Chest] [Int] deriving Show

keyToOpen (_, k, _) = k
keysInside (_, _, ks) = ks
index (i, _, _) = i

instance Problem ProblemD where
    readProblem = do
      (keyCount, chestCount) <- consumePair
      keys <- consume keyCount
      chests <- sequence $ replicate chestCount consumeChest
      let chests' = [(i, k, ks) | (i, (k, ks)) <- zip [1..] chests]
      return $ D (map read keys) chests' []
      where consumeChest = do
              [key] <- consume 1
              keys <- consumeN
              return (read key, map read keys)

    showSolution = show . solve

solve (D _ [] visited) = Just $ reverse visited
solve p   
  | null $ chestToOpen p =  Nothing
  | (not.solveable) p = Nothing
  | null solutions =  Nothing 
  | otherwise = head solutions
  where solutions = dropWhile isNothing . map solve . subproblems $ p

vis (D _ _ v) = v

subproblems p = traceShow (length $ vis p) $ map (open p) (chestToOpen p)

open (D keys chests visited) c@(i, k, ks) = D (delete k keys ++ ks) (delete c chests) (i:visited)

chestToOpen (D keys chests _) = filter canOpen chests
  where canOpen c = keyToOpen c `elem` keys

solveable (D keys chests _) = null (requiredKeys \\ allKeys)
  where allKeys = keys ++ (concat . map keysInside) chests
        requiredKeys = map keyToOpen chests

loader = loadProblems :: FilePath -> IO [ProblemD]
main = mainCodeJam loader
