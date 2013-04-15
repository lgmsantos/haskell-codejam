import CodeJam

import Data.List
import Data.Function
import Data.Maybe
import Control.Monad.State
import Debug.Trace

type Key = Int 
data ProblemD = D [Key] [(Int, Key, [Key])] [[Int]] deriving Show

cmpP = compare `on` k
    where k (D a b c) = (- length c, - length a)

keyToOpen (_, k, _) = k
keysInside (_, _, ks) = ks
index (i, _, _) = i

keysLength (D ks _ _) = length ks

compareByKeys = compare `on` keysLength

instance Problem ProblemD where
    readProblem = do
        (ks, cs) <- consumePair
        startingKeys <- consume ks
        chests <- sequence $ replicate cs consumeChests
        let chests' = [(i, k, ks) | (i, (k, ks)) <- zip [1..] chests]
        return $ D (map read startingKeys) chests' []

        where consumeChests = do
                  (k, ks) <- consumePair
                  keys <- consume ks
                  return (k, map read keys)

    showSolution = solve

loader = loadProblems :: FilePath -> IO [ProblemD]

solve p = case solve' p of
    Nothing -> "IMPOSSIBLE"
    (Just cs) -> concat . intersperse " " . map show $ cs

solve' (D [] (_:_) _) = Nothing
solve' (D _ [] r) = Just . concat . reverse $ map sort r
solve' p 
    | null subs = Nothing
    | null solutions = Nothing
    | otherwise = head solutions
    where subs = subproblems p
          solutions = dropWhile isNothing $ map solve' subs 

subproblems (D keys chests visited) = map openChests childs
    where openChests cs = (D (keysAfterOpen cs) (chests \\ cs) (map index cs : visited))
          keysAfterOpen cs = (keys \\ map keyToOpen cs) ++ (concat . map keysInside $ cs)
          childs = map nub $ combineLists [combine n (chestsForKey k) | (n, k) <- ks]
          ks = classfy keys
          chestsForKey k = filter ((== k).keyToOpen) chests

classfy lst = [(length x, head x) | x <- group lst]

main = mainCodeJam loader
combineLists [] = [[]]
combineLists (x:xs) = [x' ++ xs' | x' <- x, xs' <- combineLists xs]

combine n lst | length lst < n = []
combine 1 lst = map (:[]) lst
combine n (x:xs) = map (insert x) (combine (n - 1) xs) ++ combine n xs

--main = do
--    let p = (D [1] [(1, 1, []), (2, 1, [1, 3]), (3, 2, []), (4, 3, [2])] [])
--    print $ subproblems p