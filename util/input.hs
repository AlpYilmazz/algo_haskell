module Input where

import Data.Bifunctor ( Bifunctor(bimap) )

getListBy :: (String -> a) -> IO [a]
getListBy f = map f . words <$> getLine

getList :: IO [Int]
getList = getListBy read

getPairBy :: (String -> a) -> (String -> b) -> IO (a, b)
getPairBy f1 f2 = bimap f1 f2 . toTuple2 <$> getListBy id
    where
        toTuple2 (f:s:_) = (f, s)
        toTuple2 _ = error "Line does not have a pair"

getPair :: IO (Int, Int)
getPair = getPairBy read read
