import Control.Monad.State
import System.IO
import System.Environment
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.List

data Vine = Vine{position::Int, height::Int, next::Vine} | NoVine deriving (Eq)
instance Show Vine where
    show (Vine p h n) = show (p, h) ++ "->" ++ (show.length.asList) n
    show NoVine = "()"
reach vine = position vine + height vine
asList NoVine = []
asList vine = vine:asList (next vine)
reachableVines vine = reachableVines' $ next vine
    where reachableVines' vine' 
            | vine' == NoVine = []
            | reach vine >= position vine' = vine':reachableVines' (next vine')
            | otherwise = []
swing v v' = Vine (position v') newHeight (next v')
    where newHeight = min (position v' - position v) (height v')

data Problem = Problem{distance::Int, maxPosition::Int, vine::Vine} deriving (Eq)
instance Show Problem where
    show (Problem d m v) = "{" ++ (concat.intersperse " ") [show d, show m, show v] ++ "}"
instance Ord Problem where
    compare (Problem d m v) (Problem d' m' v') = compare (position v) (position v')

---- PARSE

readInt :: State [Int] Int
readInt = do
    (x:xs) <- get
    put xs
    return x

readIntPair :: State [Int] (Int, Int)
readIntPair = do
    (x:y:xs) <- get
    put xs 
    return (x, y)

readProblems :: State [Int] [Problem]
readProblems = do
    (problemCount:numbers) <- get
    put numbers
    sequence $ replicate problemCount readProblem

readProblem :: State [Int] Problem
readProblem = do
    vineCount <- readInt
    vine <- readVine vineCount    
    distance <- readInt
    let startingVine = Vine 0 (position vine) vine
    return $ Problem distance ((position.last.asList) vine) startingVine

readVine :: Int -> State [Int] Vine
readVine 0 = return NoVine
readVine count = do
    (position, height) <- readIntPair
    next <- readVine (count - 1)
    return $ Vine position height next

loadProblems :: String -> IO [Problem]
loadProblems filename = do
    content <- readFile filename
    let numbers = map read $ words content :: [Int]
    return $ evalState readProblems numbers

---- SOLVE

solve problem = evalState solve' [problem]

solve' :: State [Problem] Bool
solve' = do
        (problem@(Problem distance maxPosition vine):stack) <- get
        if reach vine >= distance then
            return True
        else do
            let sub = if height vine + maxPosition < distance then [] else subproblems problem
            let newStack = foldl (flip insert) stack sub
            if null newStack then
                return False
            else do
                put newStack
                solve'

subproblems (Problem d m v) = map (Problem d m) swings
    where swings = map (swing v) (reachableVines v)

allsub problem = problem:sub
    where sub = concat $ map allsub $ subproblems problem

---- PRINT

printProblems problems = do
    sequence_ $ map putStrLn $ map show' $ zip [1..] problems
    where show' (i, p) = "Case #" ++ show i ++ ": " ++ yesOrNo p
          yesOrNo p = if solve p then "YES" else "NO"

main = do
    args <- getArgs
    let filename = if null args then "../sample.in" else head args
    let problemIndex = if length args == 2 then 
                            Just $ read (args !! 1) - 1
                        else
                            Nothing :: Maybe Int
    problems <- loadProblems filename 
    start <- getCurrentTime
    printProblems $ case problemIndex of
        Just i -> [problems !! i]
        Nothing -> problems
    end <- getCurrentTime
    print $ diffUTCTime end start
