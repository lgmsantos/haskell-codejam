import CodeJam

import Data.List
import Data.Function

data ProblemC = C (Integer, Integer) deriving Show

instance Problem ProblemC where
    readProblem = do
        p <- consumePair
        return $ C p

    showSolution = show.solve

solve (C (start, end)) = end - start -- length $ fairSquares start end

isFair n = show n == (reverse.show) n

fairSquares a b = filter isFair squares
    where squares = zipWith (*) roots roots
          roots = palindromes (ceilRoot a) (floorRoot b)

ceilRoot = ceiling . sqrt . fromIntegral
floorRoot = floor . sqrt . fromIntegral

palindromes start end = takeWhile (<= end) . dropWhile (< start) . map read $ palindromes
  where startingSize = length . show $ start 
        endingSize = length . show $ end
        palindromes = concat $ map palindromes' [startingSize..endingSize]

palindromes' 1 = map show [1..9]
palindromes' n 
    | odd n = [mirr x | x <- fixes, mirr <- oddMirrors]
    | otherwise = map (mirror "") fixes
    where size = div n 2
          start = truncate (10 ** fromIntegral (size - 1))
          end = truncate (10 ** fromIntegral size)
          fixes = [start..end - 1]
          midles = map show [0..9]
          oddMirrors = map mirror midles

mirror :: String -> Integer -> String
mirror m n = show n ++ m ++ (reverse . show) n

loader = loadProblems :: FilePath -> IO [ProblemC]

main = mainCodeJam loader
--main = do
    
--    print $ ceilRoot 100000020000201200040040102402120080021204201040040002102000020000000 
--    print $ floorRoot 100000020000201201040040202403121085121304202040040102102000020000001