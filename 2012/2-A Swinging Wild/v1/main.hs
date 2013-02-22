import System.IO 
import Data.List
import Control.Parallel
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment

type Vine = (Int, Int)
data Problem = Problem [Vine] Int deriving (Show,Eq)

instance Ord Problem where
  compare p p' = compare (reach p) (reach p')
    where reach (Problem ((p, h):_) _) = p + h

-------------  PARSE

readProblem problemString = take caseCount (buildProblems cases)
  where (caseCount:cases) = map read (words problemString) :: [Int]

buildProblems [] = []
buildProblems problemData = firstProblem:buildProblems problemData'
  where (firstProblem, problemData') = buildProblem problemData          

buildProblem (vineCount:problemData) = (Problem ((0, startingReach):vines) distance, problemData')
  where vines = buildVines $ take vineCount' problemData
        ((startingReach, _):_) = vines
        vineCount' = 2 * vineCount
        (distance:problemData') = drop vineCount' problemData

buildVines [] = []
buildVines (a:b:t) = (a, b):buildVines t

------------- SOLVE

solve (Problem [] _) = False
solve (Problem ((p, h):_) d) | p + h >= d = True
solve problem 
  | null sub = False
  | otherwise = par solveOther (solveFirst || solveOther)
  where sub = (reverse.sort) $ subProblems problem
        solveFirst = solve $ head sub
        solveOther = any solve $ tail sub

subProblems (Problem (vine:vines) distance) = [Problem vines' distance | vines' <- subVines]
    where subVines = map (swing vine) $ filter (canReach vine) (sublists vines)

canReach (p, h) ((p', _):_) = p + h >= p'
swing (p, h) ((p', h'):t) = (p', min (p' - p) h'):t

sublists [] = []
sublists l@(h:t) = l:sublists t

showProblems i (p:ps) = "Case #" ++ show i ++ ": " ++ (if solve p then "YES" else "NO") ++ "\n" ++ showProblems (i + 1) ps
showProblems _ [] = ""

main = do
  args <- getArgs
  let [fileName] = args
  contents <- readFile fileName
  let problems = readProblem contents
  start <- getCurrentTime
  putStrLn $ showProblems 1 problems
  end <- getCurrentTime
  print $ diffUTCTime end start