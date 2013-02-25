import CodeJam
import Text.Printf
import Control.Monad.State
import Data.Tuple
import Data.Maybe
import Data.List
import System.Environment

type Elem = Char
data Combination = C (Char, Char) deriving (Show)
instance Eq Combination where
    (C p) == (C p') = p == p' || p == swap p'

type Transformation = (Combination, Elem)
data Magicka = M [Transformation] [Combination] [Elem] deriving (Show)

cancel combinations (p:stack) = isJust `any` [find (== C (p, p')) combinations | p' <- stack]

solve problem = solve' problem []
solve' (M _ _ []) stack = reverse stack
solve' (M trans combs (e:es)) [] = solve' (M trans combs es) [e]
solve' (M trans combs (e:es)) (peek:stack) 
    | isJust transform = solve' (M trans combs (t:es)) stack
    | cancel combs stack' = solve' (M trans combs es) []
    | otherwise = solve' (M trans combs es) stack'
    where transform = lookup (C (e, peek)) trans
          Just t = transform
          stack' = e:peek:stack

instance Problem Magicka where
    readProblem [] = Nothing
    readProblem state = Just $ runState readProblem' state
        where readProblem' = do
                    transforms <- takeSN
                    conbinations <- takeSN
                    (_:invoke:_) <- takeS 2
                    let transforms' = [(C (e1, e2), r) | [e1, e2, r] <- transforms]
                        conbinations' = [C (e1, e2) | [e1, e2] <- conbinations]                        
                    return $ M transforms' conbinations' invoke

    showSolution p = printf "[%s]" (intercalate ", " $ map (:[]) (solve p))

loadProblemsM f = loadProblems f :: IO [Magicka]

main = getArgs >>= loadProblemsM.head >>= printProblems