
-- https://algoleague.com/contest/algorithm-competition-summer-camp-2022-foundation-upsolving-contest/problem/balanced-brackets/detail

almatch :: Char
almatch = '.'

closed :: Char -> Char
closed '(' = ')'
closed '{' = '}'
closed '[' = ']'
closed _ = undefined

isOpen :: Char -> Bool
isOpen '(' = True
isOpen '{' = True
isOpen '[' = True
isOpen _ = False

matches :: Char -> Char -> Bool
matches c1 c2
    | c2 == closed c1   = True
    | c2 == almatch     = True
    | otherwise         = False

isEmpty :: [a] -> Bool
isEmpty []  = True
isEmpty _   = False

leaveFst :: (a, b, c) -> (b, c)
leaveFst (_, b, c) = (b, c)

merge3 :: (a -> b -> c -> d) -> (a, b, c) -> d
merge3 f (a, b, c) = f a b c

isBalanced :: String -> Bool
isBalanced = merge3 result . isBalancedT
    where
        result :: Bool -> Bool -> String -> Bool
        result True _ _ = False
        result _ True [] = True
        result _ _ _ = False

        isBalancedT :: String -> (Bool, Bool, String) -- (End, Balanced, Stack)
        isBalancedT [] = (False, True, [])
        isBalancedT (c:cs)
            | end = res
            | csBalanced && isOpen c        = (True, False, [])
            | csBalanced && not (isOpen c)  = (False, False, c:stack)
            | not csBalanced && isOpen c && parenMatches    = (False, isEmpty (tail stack), tail stack)
            | not csBalanced && isOpen c && not parenMatches    = (True, False, [])
            | not csBalanced && not (isOpen c)  = (False, False, c:stack)
            | otherwise = error "How"
            where
                res@(end, csBalanced, stack) = isBalancedT cs
                parenMatches = matches c (head stack)

        

solve :: String -> Bool
solve = isBalanced

ans :: Bool -> String
ans True = "Yes\n"
ans False = "No\n"

main :: IO ()
main = interact $ ans . solve . head . words