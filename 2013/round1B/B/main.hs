import CodeJam

import Debug.Trace
import Text.Printf

data ProblemB = B Double (Double, Double) deriving Show

instance Problem ProblemB where

    readProblem = do
        [diamonds] <- consume 1
        (x, y) <- consumePair
        return $ B (read diamonds) (x, y)

    showSolution (B n (x, y)) = printf "%.6f" $ solve (x, y) n

piramidSize base = (base + 1) * base / 2

solve (x, y) n
    | x < 0 = solve (-x, y) n

solve (0, y) n
    | n >= piramidSize (y + 1) = 1
    | otherwise = 0

solve (x, y) n
    | n < minDiamonds = 0
    | n > maxDiamonds = 1
    |otherwise = coinToss (y + 1) diff
    where baseSize = piramidSize (x + y - 1) 
          minDiamonds = baseSize
          maxDiamonds = baseSize + (y + x) + y
          diff = n - baseSize

coinToss minHits tosses = minHitCases / total
    where coinToss' x = fat tosses / (fat x * fat (tosses - x)) 
          minHitCases = sum . map coinToss' $ [minHits..tosses]
          total = 2 ** tosses

fat 0 = 1
fat x = foldl1 (*) [1..x]

main = mainCodeJam (loadProblems :: FilePath -> IO [ProblemB])