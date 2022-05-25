-- https://www.hackerrank.com/challenges/super-digit/problem?isFullScreen=false

import Data.Char
import Data.Bifunctor

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

superDigit :: Int -> Int
superDigit x | x < 10 = x
superDigit x = superDigit $ digitSum x

superDigitS :: String -> String
superDigitS [x] = [x]
superDigitS xs = superDigitS . show . sum . map digitToInt $ xs

f2 :: [a] -> (a, a)
f2 (h:ht:_) = (h, ht)
f2 _ = undefined

onFst :: (a -> b) -> (a, c) -> (b, c)
onFst f (a, b) = (f a, b)

mapT :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapT f g (a, b) = (f a, g b)
-- mapT = bimap

merge2 :: (a -> b -> c) -> (a, b) -> c
merge2 f (a, b) = f a b
-- merge2 = uncurry

main :: IO ()
-- main = interact $ show . superDigit . read . concat . uncurry (flip replicate) . onFst show . f2 . input
main = interact $ show . superDigit . uncurry (*) . bimap sumChars read . f2 . words
    where
        -- input = map (read :: String -> Int) . words
        sumChars = sum . map digitToInt