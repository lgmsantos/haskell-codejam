import CodeJam

import Data.List
import Debug.Trace

type Point = (Float, Int)
data HotDog = HD Float Float [Point] 
            | SD Float Float Point deriving (Show)

instance Problem HotDog where
    readProblem = do
        (count, distance) <- consumePair
        pairs <- sequence $ replicate count consumePair
        return $ HD 0 distance pairs

    showSolution = show . foldl1 merge . subProblems 

subProblems (HD time distance distribution) = map (SD time distance) distribution

merge (SD t d (p, v)) (SD t' _ (p', v')) = SD t'' d (p'', v'')
    where v'' = v + v'
          p'' = (p + p') / 2
          t'' = - (max t t' + (abs $ p - p''))

solve (SD t d (p, v)) = a / 2 + t - (d / 2)
    where a = fromIntegral v * d

loader = loadProblems :: FilePath -> IO [HotDog]
main = mainCodeJam loader
main' = do
    (_:p:_) <- loader "../sample.in"
    print p
    let sub = subProblems p
    let a = foldl1 merge sub
    print a 
    print $ solve (SD 0 2 (3, 2))
