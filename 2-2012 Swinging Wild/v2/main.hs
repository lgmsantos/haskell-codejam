import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment
import System.IO
import Data.List (sort)
import Control.Parallel

data Vine = Vine{position::Int, height::Int, next::Vine} | None deriving (Show, Eq)
instance Ord Vine where
    compare None None = EQ
    compare None _ = LT
    compare _ None = GT
    compare v v' = position v `compare` position v'
reach None = 0
reach vine = position vine + height vine
reachables vine = filter (canReach vine) $ tail $ asList vine
canReach v v' = reach v >= position v'
asList None = []
asList vine = vine:asList (next vine)

data Problem = Problem Vine Int deriving (Show)
instance Eq Problem where
    (Problem v _) == (Problem v' _) = v == v'
instance Ord Problem where
    compare (Problem v _) (Problem v' _) = compare v v'

---- PARSE

readProblems string = buildProblems problemCount numbers
    where (problemCount:numbers) = map read $ words string :: [Int]

buildProblems 0 _ = []
buildProblems problemCount (vineCount:numbers) 
    = (Problem firstVine distance):buildProblems (problemCount - 1) numbers'
    where vine = readVine vineCount numbers
          firstVine = Vine{position=0, height=position vine, next=vine}
          distance = numbers !! x
          numbers' = drop (x + 1) numbers
          x = 2 * vineCount

readVine 0 _ = None
readVine vineCount (p:h:numbers) = Vine{position=p, height=h, next=next}
    where next = readVine (vineCount - 1) numbers

---- SOLVE

solve problem@(Problem v d)
    | reach v >= d = True
    | height v + maxPosition < d = False
    | otherwise = solve `any` subproblems problem
    where maxPosition = maximum $ map position $ asList v

subproblems (Problem v d) = map constructor vs
    where constructor = flip Problem $ d
          vs = swings v

swings vine = map swing' vines
    where swing' = swing vine
          vines = reverse.sort $ reachables vine

swing v v' = Vine{position=position v', height=newHeight, next=next v'}
    where newHeight = min (position v' - position v) (height v')

---- PRINT

showProblem _ [] = ""
showProblem count (p:problems) =
    "Case #" ++ show count ++ ": " ++ yesOrNo ++ nextCase
    where yesOrNo = (if solve p then "YES" else "NO") ++ "\n"
          nextCase = showProblem (count + 1) problems

main = do
    args <- getArgs
    let filename = if null args then "../sample.in" else head args
    content <- readFile filename
    let problems = readProblems content
    start <- getCurrentTime
    putStrLn $ showProblem 1 problems
    end <- getCurrentTime
    print $ diffUTCTime end start