import CodeJam

data ProblemD = D deriving Show

instance Problem ProblemD where

	readProblem = do
		return D

main = mainCodeJam (loadProblems :: FilePath -> IO [ProblemD])