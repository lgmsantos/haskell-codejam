import Data.List
import Data.Maybe

import CodeJam

type Vendor = (Float, Float)
data HotDog = HD [Vendor] deriving (Show)

instance Problem HotDog where
    readProblem = do
        (count, distance) <- consumePair
        let d = distance / 2
        vendors <- sequence . replicate count $ consumePair
        let center = concat . map (uncurry $ flip replicate) $ vendors
        return $ HD [(c - d, c + d) | c <- center]

    showSolution = show . solve

solve (HD vendors) = maximum . map solve' . groupVendors $ vendors
solve' vendors = maximum $ zipWith vendorDiff vendors base
    where vendorDiff (a, _) (a', _) = abs $ a - a'
          base = baseVendors vendors

baseVendors vendors = [(i, i + ds) | i <- [s,s + ds..e]]
    where ds = (uncurry $ flip (-)) . head $ vendors
          s = start vendors 
          e = end vendors - ds

groupVendors vendors = snd . fromJust . find fst . iterate (merge'.snd) . merge' $ subgroups
    where subgroups = map (:[]) vendors
          merge' g = let g' = merge g in
                     (g == g', g')
          merge [g] = [g]
          merge (g:g':gs)
            | g `overlap` g' = (g ++ g'):gs
            | otherwise = g:merge (g':gs)
          overlap g g' 
            | start g > start g' = g' `overlap` g
            | otherwise = end g >= end g'

start g = midle g - area g / 2
end g = midle g + area g / 2
midle g = ((fst.head) g + (snd.last) g) / 2
area = sum . map (uncurry $ flip (-))

loader f = loadProblems f :: IO [HotDog]
main = mainCodeJam loader