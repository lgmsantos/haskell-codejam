module CodeJam(Problem(..), takeS, takeSN, mainCodeJam)
where

import Data.List
import Text.Printf
import Control.Monad.State
import System.Environment

takeS :: Int -> State [String] [String]
takeS n = do
    state <- get
    put $ drop n state
    return $ take n state

takeSN :: State [String] [String]
takeSN = do
    (n:state) <- get
    put state
    takeS (read n)

mainCodeJam :: (Problem a) => (FilePath -> IO [a]) -> IO ()
mainCodeJam loader = getArgs >>= loader.head >>= printProblems

class Problem a where
    readProblem :: State [String] a

    readProblem :: [String] -> Maybe (a, [String])
    showSolution :: a -> String

    readProblems :: [String] -> [a]
    readProblems (count:content) = unfoldr readProblem content

    loadProblems :: FilePath -> IO [a]
    loadProblems = (return.readProblems.words =<<) . readFile

    printProblems :: [a] -> IO ()
    printProblems problems = mapM_ putStrLn $ zipWith printProblem ([1..]::[Int]) problems
        where printProblem i p = printf "Case #%d: %s" i (showSolution p)