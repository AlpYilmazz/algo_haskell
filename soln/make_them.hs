import Control.Arrow

-- https://algoleague.com/contest/algorithm-competition-summer-camp-2022-foundation-upsolving-contest/problem/make-them-different/detail


opt :: Int -> Int -> String
opt f n = [head $ show ((i+f) `mod` 2) | i <- [0..n-1]]

dist :: String -> String -> Int
dist = distT 0
    where
        distT :: Int -> String -> String -> Int
        distT pts [] [] = pts
        distT pts (g:gs) (c:cs)
            | g == c    = distT pts gs cs
            | otherwise = distT (pts+1) gs cs
        distT _ _ _ = undefined

solve :: Int -> String -> Int
solve n s = min (dist (opt 0 n) s) (dist (opt 1 n) s)

pairHead :: [a] -> (a, a)
pairHead (x1:x2:_) = (x1, x2)
pairHead _ = error "not enough items"

main :: IO ()
main = interact $ show . uncurry solve . first read . pairHead . words