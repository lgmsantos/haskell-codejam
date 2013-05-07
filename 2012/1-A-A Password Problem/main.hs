import CodeJam
import Data.List
import GHC.Float
import Data.Ratio


data ProblemA = A Int Int [Double] deriving Show

instance Problem ProblemA where
    
    readProblem = do
        (n, total) <- consumePair
        ps <- consume n
        return $ A n total (map read ps)

    showSolution = show . solve

solve :: ProblemA -> Float
solve problem = fromRational $ min (solveBackspace problem) (solveEnter problem)

solveEnter (A n total _) = fromIntegral $ total + 2

solveBackspace (A n t ds) = minimum probs
    where (n', t') = (fromIntegral n, fromIntegral t)
          probs = zipWith f [n', n' - 1 .. 0] (scanl (*) 1 (map toRational ds))
          f bksp succProb = let k = bksp + bksp + (t' - n') + 1 in
                            succProb * k + (1 - succProb) * (k + t' + 1)

main = mainCodeJam (loadProblems :: FilePath -> IO [ProblemA])