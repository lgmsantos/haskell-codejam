import Control.Monad.State
import System.IO
import System.Environment
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.List

data Vine = Vine {position::Int, height::Int, maxHeight::Int} deriving (Show)
data Problem = Problem {distance::Int, vines::[Vine]} deriving (Show)

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
    vines <- sequence $ replicate vineCount readVine
    let p = position $ head vines
    distance <- readInt
    return $ Problem distance ((Vine 0 p p):vines) 

readVine :: State [Int] Vine
readVine = do
    (p, h) <- readIntPair
    return (Vine p h 0)

loadProblems :: String -> IO [Problem]
loadProblems filename = do
    content <- readFile filename
    let numbers = map read $ words content :: [Int]
    return $ evalState readProblems numbers

---- SOLVE

solve problem = solve' `any` takeWhileNotNull subproblems
    where solve' (Problem distance []) = False
          solve' (Problem distance ((Vine p h m):_)) = p + m >= distance
          subproblems = iterate subproblem problem
          takeWhileNotNull = takeWhile (not.null.vines)

subproblem (Problem distance (v:vs)) = Problem distance $ map swing' $ map swing' vs
    where swing' = swing v

swing (Vine p h m) (Vine p' h' m') = Vine p' h' (max m' m'')
    where m'' = if p + m >= p' then min (p' - p) h' else 0

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
