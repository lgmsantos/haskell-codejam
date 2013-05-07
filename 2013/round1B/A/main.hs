import CodeJam

import Data.List
import Debug.Trace

data ProblemA = A Integer [Integer] deriving Show

instance Problem ProblemA where

    readProblem = do
        [mymote] <- consume 1
        motes <- consumeN
        return $ A (read mymote) (sort $ map read motes)

    showSolution = show . solve

solve (A mote motes) = solve' (A mote motes) 0

solve' (A _ []) size = size
solve' (A mote (m:ms)) size
    | m < mote = solve' (A (mote + m) ms) size
    | newMote == 0 = solveRemoving
    | otherwise = min solveAdding solveRemoving
    where newMote = mote - 1
          solveAdding = solve' (A (mote + newMote) (m:ms)) (size + 1)
          solveRemoving = size + length (m:ms) -- solve' (A mote ms) (size + 1)

main = mainCodeJam (loadProblems :: FilePath -> IO [ProblemA])