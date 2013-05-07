import Control.Monad
import Data.List
import Debug.Trace
import Data.Function
import CodeJam

type Point = (Float, Int)
data HotDog = HD Float [Point] deriving (Show)

instance Problem HotDog where
    readProblem = do
        (count, distance) <- consumePair
        pairs <- replicateM count consumePair
        return $ HD distance pairs

    showSolution = show . solve 

solve = maximum . map solve' . subproblems
solve' problem = abs . distance flat . adjustFrom flat base . problem
    where flat = flatten problem

subproblems (HD distance points) = 

range distance points = (mid - r, mid + r)
    where mid = midle . fst . unzip $ points
          r = area distance points / 2

flatten (HD distance points) = concat $ points >>= return . (uncurry $ flip replicate)

base p@(HD distance points) = [0,distance..area distance points]

area distance points = (distance *) . fromIntegral . pred . sum . snd . unzip $ points

distance p p' = maximumBy (compare `on` abs) $ zipWith (-) p p'

adjustFrom original points = map (+ (m - m')) points
    where m = midle original
          m' = midle points

midle points = (head points + last points) / 2



info p@(HD d pts) = (head f - ar, midle f, last f + ar)
  where f = flatten p
        ar = area d pts / 2

loader = loadProblems :: FilePath -> IO [HotDog]
main = mainCodeJam loader