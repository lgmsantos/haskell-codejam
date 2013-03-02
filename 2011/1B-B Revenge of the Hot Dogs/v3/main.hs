import Data.List

import CodeJam

data HotDog = HD Float [Float] deriving (Show)

instance Problem HotDog where
    readProblem = do 
        (pointCount, distance) <- consumePair
        pairs <- sequence $ replicate pointCount consumePair
        let pairs' = concat $ map (uncurry $ flip replicate) pairs
        return $ HD distance pairs'

    showSolution p = show $ abs . problemDistance p $ search p

search original = result
    where Just (result, _) = find scoreFind $ zip iter (tail iter) 
          iter = iterate (adjustBy original) (base original)
          scoreFind (p, p') = (abs.problemDistance original) p < (abs.problemDistance original) p'

base (HD dist vendors) = (HD dist [0,dist..dist * (genericLength vendors - 1)])

problemDistance (HD _ v) (HD _ v') = maximumBy abscompare $ zipWith (-) v v'

abscompare a b = abs a `compare` abs b

adjustBy original problem@(HD d v) = HD d v'
    where v' = map (+ adjustValue) v
          adjustValue = problemDistance original problem / 2

shift val (HD d v) = HD d (map (+ val) v)

loader :: FilePath -> IO [HotDog]
loader = loadProblems
main = mainCodeJam loader