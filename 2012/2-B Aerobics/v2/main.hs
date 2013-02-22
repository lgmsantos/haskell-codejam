import System.IO (readFile)
import System.Environment (getArgs)
import Control.Monad.State
import Data.List
import Data.Maybe

data Problem = Problem Int Int [Int] deriving (Show)

---- PARSE

buildProblem :: State [Int] Problem
buildProblem = do
    (count:w:h:numbers) <- get
    let radiuses = take count numbers
    put $ drop count numbers
    return (Problem w h radiuses)

loadFile :: String -> IO [Problem]
loadFile filename = do
    content <- readFile filename
    let (count:numbers) = map read $ words content :: [Int]
    return $ evalState (sequence $ replicate count buildProblem) numbers

---- SOLVE

solve :: Problem -> [(Int, Int)]
solve (Problem width height radiuses) 
    | not.isNothing $ find ((height <).snd) result = error $ show (height, find ((height <).snd) result)
    | not.isNothing $ find ((width <).fst) result = error $ show (width, find ((width <).fst) result)
    | otherwise = result
    where result = [(x, y) | (y, xs) <- zip posY posX, x <- xs]
          rows = groupBySum width radiuses
          posX = map distribute rows
          posY = distribute.head.transpose $ rows

space (x:[]) = 0
space (x:xs) = x + (sum.init) xs + sum xs

distribute xs = 0:(map sum $ map sumPairs.tail.inits $ zip xs (tail xs))
    where sumPairs = map $ uncurry (+)

groupBySum width list = unfoldr group' list
    where group' seed 
            | null seed = Nothing
            | space seed <= width = Just (seed, [])
            | otherwise = Just (value, nextSeed)
            where sumLimit = (width <) . space
                  value = init.fromJust.find sumLimit.tail.inits $ seed
                  nextSeed = drop (length value) seed

---- PRINT

printProblems problems = sequence_ $ map putStrLn $ map (uncurry print') $ zip [1..] problems
    where print' i problem = concat ["Case #", show i, ": ", strProblem problem]
          strProblem p = concat $ intersperse " " $ map showPair $ solve p
          showPair (x, y) = concat [show x, " ", show y]

main = do
    args <- getArgs
    let filename = if null args then "../sample.in" else head args
    problems <- loadFile filename
    printProblems problems