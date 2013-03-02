import CodeJam
import System.Environment
import Control.Monad.State
import Data.Bits
import Data.List

data CandySplitting = CS [Int] deriving (Show)

solve :: CandySplitting -> Maybe Int
solve (CS values) 
    | sumXOR /= 0 = Nothing
    | otherwise = Just . sum . tail $ values 
    where sumXOR = foldl1 xor values

instance Problem CandySplitting where
    readProblem = consumeN >>= return . CS . sort . map read
    showSolution problem = case solve problem of
        Nothing -> "NO"
        Just n -> show n
    --showSolution (CS values) = show $ (head values, foldl1 xor $ tail values)

main = mainCodeJam (loadProblems :: FilePath -> IO [CandySplitting])