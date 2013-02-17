import Control.Monad.State
import System.IO
import Data.List (sort)
import System.Environment (getArgs)
import Text.Printf

data Problem = Problem Int Int [Int] deriving (Show)

---- PARSE

readInt :: State [Int] Int
readInt = do
    (value:state) <- get
    put state
    return value

readProblems numbers = evalState readProblems' numbers

readProblems' :: State [Int] [Problem]
readProblems' = do
    caseCount <- readInt
    let readProblem = do
        (n:w:h:_) <- sequence $ replicate 3 readInt
        radiuses <- sequence $ replicate n readInt
        return $ Problem w h $ reverse $ radiuses
    sequence $ replicate caseCount readProblem

loadFile :: String -> IO [Int]
loadFile filename = do
    content <- readFile filename
    return $ map read $ words content :: IO [Int]

---- SOLVE

solve problem = [ (x, y) | (x, y, _, _) <- evalState (solve' problem) [] ]
solveTriples p@(Problem _ _ radiuses) = [ (x, y, r) | (r, (x, y)) <- zip radiuses (solve p)]

solve' :: Problem -> State [(Int, Int, Int, Int)] [(Int, Int, Int, Int)]
solve' (Problem w h radiuses) = do
    sequence_ $ map solve'' radiuses
    state <- get
    return $ reverse state
    where solve'' radius = do
          state <- get
          if null state then
              put [(0, 0, radius, radius)]
          else do            
              let (x, y, ry, r):_ = state
              let nextX = x + r + radius
              let nextY = y + ry + radius
              if nextX <= w then
                  put $ (nextX, y, ry, radius):state
              else if nextY <= h then
                  put $ (0, nextY, radius, radius):state
              else error "fudeu"

---- PRINT

printProblems problems = sequence_ $ map putStrLn $ map showProblem $ zip [1..] problems
    where showProblem (i, p) = concat ["Case #", show i, ": ", concat $ map showPair (solve p)]
          showPair (x, y) = concat [show x, " ", show y, " "]

printSvgSquares :: Problem -> IO ()
printSvgSquares p@(Problem w h rs) = sequence_ $ matRect:map rect rectValues
  where triples = solveTriples p
        prop = (fromIntegral 10) / fromIntegral (minimum rs) :: Float
        scale v = truncate $ fromIntegral v * prop :: Int
        matRect = printf "<rect x='%d' y='%d' width='%d' height='%d' style='fill:None; stroke-width:1; stroke:red' />\n" (scale translate) (scale translate) (scale w) (scale h) :: IO ()
        rect (x, y, width, height) = printf "<rect x='%d' y='%d' width='%d' height='%d' style='fill:None; stroke-width:1; stroke:black' />\n" (scale (x + translate)) (scale (y + translate)) (scale width) (scale height)
        rectValues = [(x - r, y - r, r * 2, r * 2) | (x, y, r) <- triples] 
        translate = maximum [div r 2 | (_, _, r, _) <- rectValues] :: Int

main = do
    args <- getArgs
    --let filename = if null args then "../sample.in" else head args
    let filename = "../B-small-practice.in"
    numbers <- loadFile filename
    let problems = readProblems numbers
    --printProblems problems
    --validate problems
    let p = problems !! 3
    printSvgSquares p
    print p
    print $ solve p
