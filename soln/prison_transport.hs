-- https://www.hackerrank.com/challenges/prison-transport/problem?isFullScreen=false

import Data.Map as M hiding (foldl, union, map, foldr)
import Data.Maybe ( fromJust, isNothing )
import Data.List hiding (union, find)
import Control.Monad

type Dst = M.Map Int Int

find :: Int -> Dst -> (Int, Dst)
find key mp
    | isNothing val    = undefined
    | val == Just key   = (key, mp)
    | otherwise         = fmap (M.insert key lead) ret
    where
        val = M.lookup key mp
        ret@(lead, _) = find (fromJust val) mp

union :: Int -> Int -> Dst -> Dst
union k1 k2 mp = M.insert lead1 lead2 mpr2
    where
        (lead1, mpr1) = find k1 mp
        (lead2, mpr2) = find k2 mpr1

groupsOf :: Dst -> [Int]
groupsOf mp = map length . group . sort . M.elems $ mpNew
    where
        findThenSnd :: Int -> Dst -> Dst
        findThenSnd k m = snd $ find k m
        mpNew = foldr findThenSnd mp (M.keys mp)

ceilSqr :: Int -> Int
ceilSqr = ceiling . sqrt . (fromIntegral :: Int -> Float)

solve :: Int -> [(Int, Int)] -> Int
solve n = sum . map ceilSqr . groupsOf . foldr insertR mpStart
    where
        insertR = uncurry union
        mpStart = M.fromList $ zip [1..n] [1..n]

pairUp :: [a] -> [(a, a)]
pairUp [] = []
pairUp (x1:x2:xs) = (x1, x2) : pairUp xs
pairUp _ = undefined

parse :: [String] -> (Int, Int, [(Int, Int)])
parse (n:m:pairs) = (read n, read m, pairUp (map read pairs))
parse _ = undefined

leaveSnd :: (a, b, c) -> (a, c)
leaveSnd (a, _, c) = (a, c)

getInput :: IO [String]
getInput = do
    n <- getLine
    m <- getLine
    pairs <- replicateM (read m) getLine -- ["1 2", "2 3"]
    let ps = join $ map words pairs  -- ["1", "2", "2", "3"]
    return (n : m : ps)

main :: IO ()
-- main = interact $ show . uncurry solve . leaveSnd . parse . words
main = do
    ws <- getInput
    let ans = show . uncurry solve . leaveSnd . parse $ ws
    putStrLn ans
    return ()

