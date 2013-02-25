import Data.List
import System.Environment

type RobotPosition = (Int, Int)
data Robot = Orange | Blue deriving (Show, Eq)

getPosition Orange = snd
getPosition Blue = fst

--- PARSE

wordsFromFile :: FilePath -> IO [String]
wordsFromFile = (return.words =<<) . readFile

readProblems :: FilePath -> IO [[(Robot, Int)]]
readProblems filePath = do
    ws <- wordsFromFile filePath
    return $ unfoldr f (tail ws)
    where f [] = Nothing
          f (count:commands) = let (commands', reminder) = splitAt (2 * read count) commands
                                   pairs [] = Nothing
                                   pairs (a:b:c) = let robot = case a of { "O" -> Orange; "B" -> Blue} 
                                                       button = read b in
                                                   Just ((robot, button), c) in
                               Just (unfoldr pairs commands', reminder)

---- SOLVE

solve :: [(Robot, Int)] -> Int
solve cmds = solve' cmds (1, 1) 0
solve' [] _ ticks = ticks
solve' cmds pos ticks = solve' cmds' pos' (ticks + 1)
    where pressButton (robot, button) = (getPosition robot pos) == button
          pos' = moveRobots cmds pos
          cmds' = if pressButton $ head cmds then tail cmds else cmds

moveRobots cmds pos = (moveRobot Blue, moveRobot Orange)
    where moveRobot robot = case find ((== robot).fst) cmds of 
                               Just (_, p) -> move (getPosition robot pos) p
                               Nothing -> getPosition robot pos
          move orig dest
            | orig > dest = orig - 1
            | orig < dest = orig + 1
            | otherwise = orig

---- PRINT

printProblems problems = sequence_ $ map putStrLn $ zipWith showProblem [1..] problems
    where showProblem i p = "Case #" ++ show i ++ ": " ++ show (solve p)

getFilename = do
    args <- getArgs
    return $ if null args then "../sample.in" else head args

main = do
    filename <- getFilename
    ps <- readProblems filename
    printProblems ps