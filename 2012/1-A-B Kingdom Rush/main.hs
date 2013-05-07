import CodeJam
import Control.Monad
import Data.List
import Data.Function
import Data.Tuple
import Debug.Trace

data Stage = Stage0 (Int, Int) | Stage1 Int | Stage2 deriving (Show, Eq, Ord)

totalPlay (Stage0 _) = 0
totalPlay (Stage1 _) = 1
totalPlay Stage2 = 2

twoStars Stage2 = True
twoStars _ = False

newStage (r1, r2) = Stage0 (r2, r1)

compareStage _ Stage2 Stage2 = EQ
compareStage _ _ Stage2 = LT
compareStage t s s' = (compare `on` (outcome t)) s s'

outcome t (Stage0 (r2, r1))
    | t >= r2 = (-2, 0)
    | t >= r1 = (-1, 1)
    | otherwise = (0, 2)

outcome t (Stage1 r2)
    | t >= r2 = (-1, 0)
    | otherwise = (0, 1)

outcome t Stage2 = (0, 2)

data ProblemB = B [Stage] deriving Show

instance Problem ProblemB where
    readProblem = do
        [n] <- return.map read =<< consume 1
        pairs <- replicateM n consumePair
        return . B $ map newStage pairs

    showSolution p = case solve p of
                    Just n -> show n
                    Nothing -> "Too Bad"

play t s@(Stage0 (r2, r1))
    | t >= r2 = (Stage2, 2)
    | t >= r1 = (Stage1 r2, 1)
play t s@(Stage1 r2)
    | t >= r2 = (Stage2, 1)
play _ s = (s, 0)

solve (B stages) = solve' stages 0 0
solve' stages total plays
    | all twoStars stages = Just plays
    | t' == 0 = Nothing    
    | otherwise = solve' (s':ss) (total + t') (plays + 1)
    where (s:ss) = sortBy (compareStage total) stages
          (s', t') = play total s

main = mainCodeJam (loadProblems :: FilePath -> IO [ProblemB])
