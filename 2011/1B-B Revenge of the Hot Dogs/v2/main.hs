import CodeJam
import Data.List
import Data.Maybe
import Debug.Trace
import Data.Ratio
import Data.Functor

type Point = (Float, Int)
data HotDog = HD Float [Point] deriving (Show)

solve problem@(HD distance points) = trace (show originalDistribution ++ "\n" ++ show finalDistribution) $ maximum $ map abs times
    where times = zipWith (-) originalDistribution finalDistribution
          originalDistribution = concat $ map (uncurry $ flip replicate) points
          finalDistribution = concat $ map (flatten distance) groups
          groups = groupByRange problem

flatten distance points = result
    where result = takeWhile (<= end) $ map (+ start) [0,distance..]
          (start', end') = getRange distance points
          start = start' + distance / 2
          end = end' - distance / 2

groupByRange (HD distance points) = iterate (map concat . groupBy overlap) groups !! 100
    where groups = map (:[]) points
          overlap p p' = overlap' (getRange distance p) (getRange distance p')
          overlap' (a, b) (c, d)
            | c < a = overlap' (c, d) (a, b)
            | otherwise = c < b

getRange distance points = (mid - (area / 2), mid + (area / 2))
    where (locations, vendors) = unzip points
          mid = (maximum locations - minimum locations) / 2
          area = fromIntegral (sum vendors) * distance

average list = sum list / genericLength list

instance Problem HotDog where
    readProblem = do
        (pointCount, distance) <- consumePair
        pairs <- sequence $ replicate pointCount consumePair
        return $ HD distance pairs

    showSolution = show . solve

loader = loadProblems :: FilePath -> IO [HotDog]

main = do
    mainCodeJam loader