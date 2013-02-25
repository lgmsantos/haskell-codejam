import Data.Functor
import Data.List
import CodeJam

data Robot = Orange | Blue deriving (Show, Eq)
type Command = (Robot, Int)
data BotTrust = BT (Int, Int) [Command] deriving (Show)

robotPosition Blue = fst
robotPosition Orange = snd

readBot "B" = Blue
readBot "O" = Orange
readCommand [] = Nothing
readCommand (r:b:xs) = Just ((readBot r, read b), xs)

solve problem = solve' problem 0
    where solve' (BT _ []) ticks = ticks
          solve' problem ticks = solve' (tickProblem problem) (ticks + 1)

tickProblem (BT pos cmds) = BT (posBlue, posOrange) cmds'
    where (currentRobot, targetButton) = head cmds
          cmds' = if robotPosition currentRobot pos == targetButton then tail cmds else cmds
          posBlue = moveRobot Blue
          posOrange = moveRobot Orange
          moveRobot robot = case find ((== robot).fst) cmds of
                                Just (_, b) -> move (robotPosition robot pos) b
                                Nothing -> robotPosition robot pos
          move orig dest 
            | orig < dest = orig + 1
            | orig > dest = orig - 1
            | otherwise = orig


instance Problem BotTrust where
    readProblem [] = Nothing
    readProblem (x:xs) = Just (BT (1, 1) commands, otherContent)
        where size = 2 * read x            
              (content, otherContent) = splitAt size xs
              commands = unfoldr readCommand content

    showSolution problem = show $ solve problem

main = return "../sample.in" >>= loadProblems >>= (printProblems :: [BotTrust] -> IO ())