import Data.Function
import Debug.Trace

import CodeJam

type Vendor = (Float, Float, Float)
data HotDog = Single Float Vendor 
            | Composite HotDog HotDog 
            | Raw [HotDog]  deriving (Show)

solve (Raw hotdogs) =  foldl1 mergeHotdogs $ hotdogs

problemTime (Single t _) = t
problemTime (Composite h h') = maxTime h h'
    where maxTime = max `on` problemTime

mergeHotdogs h@(Single _ (_, _, e)) hc'@(Composite h'@(Single _ (s, _, _)) hc)
    | e <= s = Composite h hc'
    | h `overlap` h' = mergeHotdogs (h `mergeHotdogs` h') hc
    | otherwise = mergeHotdogs h' (h `mergeHotdogs` hc)

mergeHotdogs (Composite h hc) h'@(Single _ _) 
    | h `overlap` h' = mergeHotdogs (h `mergeHotdogs` h') hc
    | otherwise = mergeHotdogs h (h' `mergeHotdogs` hc)    

mergeHotdogs h@(Single t v@(s, _, e)) h'@(Single t' v'@(s', _, e'))
    | h `overlap` h' = Single t'' v''
    | otherwise = Composite h h'
    where v'' = mergeVendors v v'
          (s'', _, e'') = v''
          t'' = max t t' + maxAbs (s'' - (min s s')) (e'' - (max e e''))
          maxAbs = max `on` abs

overlap (Single _ v) (Single _ v') = overlap' v v'
    where overlap' (s, _, e) (s', _, e')
            | s > s' = overlap' (s', 0, e') (s, 0, e)
            | otherwise = s' < e

mergeVendors (s, m, e) (s', m', e') = (s'', m'', e'')
    where m'' = (m + m') / 2
          s'' = m'' - len
          e'' = m'' + len
          len = (e + e' - s - s') / 2

instance Problem HotDog where
    readProblem = do
        (count, distance) <- consumePair
        vendors <- sequence $ replicate count consumePair
        let d = distance / 2
            vendors' = [(m - d, m, m + d) | m <- concat $ map (uncurry $ flip replicate) vendors]
        return $ Raw $ map (Single 0) vendors'

    showSolution = show . solve

loader = loadProblems :: FilePath -> IO [HotDog]
main = mainCodeJam loader

main' = do
    --let hotdogs = map (Single 0) $ [(-1, 0, 1), (-1, 0, 1), (-1, 0, 1), (1, 2, 3)]
    --print $ foldl1 mergeHotdogs hotdogs
    --print $ overlap (Single 0 (5, 6, 7)) (Single 0 (1, 3, 5))
    print $ mergeHotdogs (Single 0 (5, 6, 7)) $ mergeHotdogs (Single 0 (-1, 0, 1)) $ mergeHotdogs (Single 0 (2, 3, 4)) (Single 0 (2, 3, 4))