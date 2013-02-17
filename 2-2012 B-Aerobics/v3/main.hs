import System.IO (readFile)
import Data.List
import Text.Printf
import System.Environment

---- PARSE

loadProblems :: String -> IO [(Int, Int, [Int])]
loadProblems filename = do
    content <- readFile filename
    let (problemCount:numbers) = map read $ words content :: [Int]
        f [] = Nothing
        f (c:w:h:rs) = Just ((w, h, take c rs), drop c rs)
    return $ unfoldr f numbers

---- SOLVE

solve :: (Int, Int, [Int]) -> [(Int, Int)]
solve (width, height, radiuses) = concat rows
    where rows = unfoldr f (0, radiuses)
          f (_, []) = Nothing
          f (yAcc, rs) = let allocation = takeWhile (<= width) (allocate rs) 
                             (allocated, rs') = splitAt (length allocation) rs
                             y = if yAcc == 0 then 0 else yAcc + maximum allocated
                             yAcc' = y + maximum allocated
                             pairs = [(x, y) | x <- allocation] in
                         Just (pairs, (yAcc', rs'))
          allocate rs = 0:(map sum . tail . inits $ zipWith (+) rs (tail rs))

---- PRINT 

printProblems ps = sequence_ $ zipWith printProblem [1..] ps
    where printProblem c p = let strPair (x, y) = printf "%d %d " x y :: String
                                 strSolve = concat $ map strPair $ solve p in
                             putStrLn $ "Case #" ++ show c ++ ": " ++ strSolve

getFilename = do
    args <- getArgs
    return $ case args of 
                  [] -> "../sample.in"
                  (filename:_) -> filename

main = do
    filename <- getFilename
    problems <- loadProblems filename
    printProblems problems