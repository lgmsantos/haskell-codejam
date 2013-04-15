import CodeJam
import Data.List

data ProblemA = A [[Char]] deriving Show

instance Problem ProblemA where
    readProblem = do
        lns <- consume 4
        return $ A lns

    showSolution = solve

solve (A matrix) = if testWin 'O' then
                       "O won"
                   else if testWin 'X' then 
                       "X won"
                   else if hasEmptySlots then
                       "Game has not completed"
                   else 
                       "Draw"

    where testWin char = testWin' char matrix || testWin' char (transpose matrix) 
          testWin' char m = elem (replicate 4 char) $ map (replace 'T' char) (m ++ diagonals m)
          hasEmptySlots = any (elem '.') matrix

diagonals m = [fst d, snd d]
    where d = unzip [(lst !! i, lst !! (length lst - i - 1)) | (i, lst) <- zip [0..] m]

replace c c' lst = [ if x == c then c' else x | x <- lst ]

loader = loadProblems :: FilePath -> IO [ProblemA]

main = mainCodeJam loader