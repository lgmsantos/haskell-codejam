import CodeJam

import Data.List
import Data.Maybe
import Debug.Trace
import Data.Ratio

data ProblemA = A Integer Integer deriving Show

instance Problem ProblemA where

    readProblem = do
        (r, t) <- consumePair
        return $ A r t

    showSolution = show . solve

solve (A r t) = binSearch 0 100000000000000000000
    where f x = 2*x*x + 2*r*x - x
          binSearch s e 
              | e - s <= 1 = m
              | f m < t = binSearch m e
              | f m > t = binSearch s m
              | otherwise = m    
              where m = (e + s) `div` 2

main = mainCodeJam (loadProblems :: FilePath -> IO [ProblemA])