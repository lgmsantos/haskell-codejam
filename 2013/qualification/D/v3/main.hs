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

    showSolution = solve

solve p@(D keys chests _) 
  | null solutions = "IMPOSSIBLE"
  | otherwise = concat . intersperse " " . map show $ solution
  where solution = map index $ head solutions
        solutions = filter (solve' p) permuts
        permuts = perm chests

solve' (D keys chests _) solution = solve'' keys solution
  where solve'' _ [] = True
        solve'' [] _ = False
        solve'' keys ((_, k, ks):xs) 
          | not $ elem k keys = False
          | otherwise = solve'' (delete k keys ++ ks) xs

perm [x] = [[x]]
perm lst = [x : xs | x <- lst, xs <- perm (delete x lst)]

loader = loadProblems :: FilePath -> IO [ProblemD]
main = mainCodeJam loader
