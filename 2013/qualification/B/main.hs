import CodeJam

import Data.List 
import Data.Function

data D = H | V deriving (Show, Eq, Ord)

type Action = (Int, Int, D)

data ProblemB = B [[Int]] deriving Show

instance Problem ProblemB where
    readProblem = do
        (h, w) <- consumePair
        ints <- sequence $ replicate h $ consume w
        return $ B (map (map read) ints)

    showSolution = solve

solve (B target) = if target == foldl (flip ($)) lawn (map apply acts) then "YES" else "NO"
    where acts = actions target
          lawn = replicate (length target) $ replicate (length.head $ target) 100

apply (g, i, H) lawn = [if j == i then map (min g) x else x
                        | (j, x) <- zip [0..] lawn]

apply (g, i, V) lawn = transpose . apply (g, i, H) . transpose $ lawn

actions matrix = reverse . sort $ hs ++ vs 
    where hs = lst H matrix
          vs = lst V (transpose matrix)
          lst cons m = [(g, i, cons) | (g, i) <- zip (actions' m) [0..]]
          actions' = map maximum

loader = loadProblems :: FilePath -> IO [ProblemB]

main = mainCodeJam loader