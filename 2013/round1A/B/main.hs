import CodeJam

import Data.List

data ProblemB = B Int Int [Int] deriving Show

instance Problem ProblemB where

    readProblem = do
        (maxE, recE) <- consumePair
        [acts] <- consume 1
        values <- consume (read acts)
        return $ B maxE recE (map read values)

    showSolution = show . solve

solve (B e r tasks) = solve' tasks e 0
    where solve' [] _ t = t
          solve' (x:xs) e' t = maximum $ [solve' xs (min e (e' - v + r)) (t + v * x) | v <- [0..e']]

main = mainCodeJam (loadProblems :: FilePath -> IO [ProblemB])