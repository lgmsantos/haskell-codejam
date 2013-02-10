import Control.Monad.State

data Vine = Vine{position::Int, height::Int, next::Vine} | NoVine
instance Show Vine where
    show (Vine p h n) = show (p, h) ++ "->" ++ show n
    show NoVine = "()"

data Problem = Problem{vine::Vine, distance::Int}
    deriving (Show)

---- Parse

readInt :: State [Int] Int
readInt = get >>= \(r:rs) -> put rs >> return r

readIntPair :: State [Int] (Int, Int)
readIntPair = get >>= \(x:x':xs) -> put xs >> return (x, x')

readProblem :: State [Int] Problem
readProblem = do
    vineCount <- readInt
    vine <- readVine vineCount    
    distance <- readInt
    let startingVine = Vine 0 (position vine) vine
    return $ Problem startingVine distance

readVine :: Int -> State [Int] Vine
readVine 0 = return NoVine
readVine count = do
    (position, height) <- readIntPair
    next <- readVine (count - 1)
    return $ Vine position height next

---- Solve



main = do
    --print $ evalState readVine [1, 1]
    print $ evalState readProblem [3, 3, 4, 4, 10, 6, 10, 3]
