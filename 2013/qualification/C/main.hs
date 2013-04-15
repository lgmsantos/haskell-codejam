import CodeJam

data ProblemC = C (Integer, Integer) deriving Show

instance Problem ProblemC where
    readProblem = do
        p <- consumePair
        return $ C p

    showSolution = show.solve

solve (C (start, end)) = length . filter isFair $ squares start end

isFair n = show n == (reverse.show) n

squares a b = zipWith (*) lst lst
    where lst = filter isFair [ceilRoot a .. floorRoot b]

ceilRoot = ceiling . sqrt . fromIntegral
floorRoot = floor . sqrt . fromIntegral

loader = loadProblems :: FilePath -> IO [ProblemC]

main = mainCodeJam loader