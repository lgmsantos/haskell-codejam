module CodeJam(Problem(..), consume, consumeN, mainCodeJam, consumePair)
where

import Data.List
import Text.Printf
import Control.Monad.State
import System.Environment

consume :: Int -> State [String] [String]
consume n = state (splitAt n)

consumeN = consume 1 >>= consume . read . head

consumePair :: (Read a, Read b) => State [String] (a, b)
consumePair = consume 2 >>= return . toPair
    where toPair [a, b] = (read a, read b)

mainCodeJam :: (Problem a) => (FilePath -> IO [a]) -> IO ()
mainCodeJam loader = getArgs >>= loader.head >>= printProblems

class Problem a where
    readProblem :: State [String] a
    showSolution :: a -> String

    readProblems :: [String] -> [a]
    readProblems (count:content) = evalState actions content
        where actions = sequence $ replicate (read count) readProblem

    loadProblems :: FilePath -> IO [a]
    loadProblems path = readFile path >>= return.readProblems.words

    printProblems :: [a] -> IO ()
    printProblems problems = mapM_ putStrLn $ zipWith printProblem ([1..]::[Int]) problems
        where printProblem i p = printf "Case #%d: %s" i (showSolution p)