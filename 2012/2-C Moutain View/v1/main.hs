import Data.List
import Data.Ix
import System.Environment
import System.IO


loadProblems :: String -> IO [[Int]]
loadProblems filename = do
    content <- readFile filename
    let (_:numbers) = map read $ words content :: [Int]
    return $ map (map pred) $ unfoldr f numbers
    where f [] = Nothing
          f (count:numbers) = Just $ splitAt (count - 1) numbers

getFilename :: IO String
getFilename = do
    args <- getArgs
    return $ if null args then "../sample.in" else head args

getPrintType :: IO ([[Int]] -> IO ())
getPrintType = do
    args <- getArgs
    if length args == 2 then
      return printPlot
    else
      return printProblems

solve :: [Int] -> Maybe [Int]
solve problem
    | isImpossible problem = Nothing
    | otherwise = Just $ foldl (flip ($)) initial functions
    where base = 10
          initial = [base,2 * base..base * (length problem + 1)]
          functions = zipWith func [0..] problem          

isImpossible problem = testOverlap `any` pairCombinations
  where pairs = zip [0..] problem
        pairCombinations = tail $ inits pairs
        overlap (a, b) (c, d) = c < b && b < d
        testOverlap (p:ps) = overlap p `any` ps

func :: Int -> Int -> [Int] -> [Int]
func x m ms = map f [0..length ms - 1]
    where lineFunc x = ceiling $ fromIntegral x * a + b
          (maxY, maxX) = maximum $ zip (drop (x + 1) ms) [x + 1..]
          a = fromIntegral (maxY - ms !! x) / fromIntegral (maxX - x) :: Float
          b = fromIntegral (ms !! x) - a * fromIntegral x
          f x'
            | x' <= x = ms !! x'
            | x' < m = lineFunc x
            | otherwise = lineFunc x'

printProblems problems = sequence_ $ map putStrLn $ zipWith showProblem [1..] problems
    where showProblem i p = "Case #" ++ show i ++ ": " ++ showProblem' (solve p)
          showProblem' Nothing = "Impossible"
          showProblem' (Just xs) = concat $ intersperse " " $ map show xs

printPlot problems = do
  (_:caseNumberStr:_) <- getArgs
  let caseNumber = read caseNumberStr
      Just solution = solve $ problems !! caseNumber
      s = [show a ++ " " ++ show b | (a, b) <- zip [1..] solution]
  sequence_ $ map putStrLn s

main = do
    filename <- getFilename
    printType <- getPrintType
    problems <- loadProblems filename
    printType problems
