import CodeJam
import Data.List

data GoroSort = GS [Int] deriving (Show)

sortDistance list = sortDistance' list (sort list) 0
    where sortDistance' [] [] acc = acc
          sortDistance' (x:xs) (x':xs') acc
            | x == x' = sortDistance' xs xs' acc
            | otherwise = sortDistance' xs xs' (acc + 1)

instance Problem GoroSort where
    readProblem = consumeN >>= return . GS . map read
    showSolution (GS values) = show $ sortDistance values

main = mainCodeJam (loadProblems :: FilePath -> IO [GoroSort])