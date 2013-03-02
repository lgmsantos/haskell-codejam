import CodeJam
import Data.List

data Group = G Float Float [(Float, Int)]
data HotDogs = HD Int [(Float, Int)] deriving (Show)

solve (HD distance (p:ps)) = solve'
  where (cd, td) = solve' distance [p]

groupPoints :: Int -> [Group] -> [Group] -> [Group]
groupPoints _ [] groups = groups
groupPoints distance (h:t) [] = groupPoints distance t [h]
groupPoints distance (h:t) (g:gs)
  | h `overlap` g = groupPoints (merge h g:t) (gs)
  | otherwise = groupPoints t (t:g:gs)

overlap (G start end _) (G start' end' _) 
  | start' <= start && 

distribution distance points = (currentDistribution, targetDistribution)
  where (ps, vs) = unzip points
        d = fromIntegral distance
        mid = avg ps
        currentDistribution = concat $ zipWith replicate vs ps
        baseDistribution = [0.0,d..(d * fromIntegral (sum vs - 1))]
        mid' = avg baseDistribution
        adjust n = n - (mid' - mid)
        targetDistribution = map adjust baseDistribution

avg list = sum list / genericLength list

instance Problem HotDogs where
    readProblem = do 
        [points, distance] <- consume 2
        pairs <- sequence $ replicate (read points) consumePair
        return $ HD (read distance) pairs   

    showSolution = show 

loadProblemsHD = loadProblems :: FilePath -> IO [HotDogs]
--main = mainCodeJam loadProblemsHD
main = do
  --problems <- loadProblemsHD "../B-small-practice.in"
  --print $ solve $ problems !! 11
  print $ distribution 2 [(0, 3), (1, 1)]