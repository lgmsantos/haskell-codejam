import CodeJam

data ProblemC = C deriving Show

instance Problem ProblemB where

	readProblem = do
		return C

main = mainCodeJam (loadProblems :: FilePath -> IO [ProblemC])