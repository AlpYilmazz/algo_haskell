-- https://www.hackerrank.com/challenges/dice-path/problem?isFullScreen=false

-- TURNS OUT NOT A DP QUESTION (ON TABLE AS KEYS)
-- OOPS
-- MAYBE WITH TABLE AND DICE ROTATION AS KEYS

-- SOLVE BOTTOM-UP
-- FILL DP FROM 1 1 TO M N
-- THEN RETURN DP ALL THE WAY
-- QUERY DP WITH M N
-- PASS DP TO NEXT CASE FOR QUICKER ANSWER

import qualified Data.Map as M
import Data.Maybe
import Control.Applicative
import Data.Bifunctor ( Bifunctor(bimap) )
import Control.Monad
import Data.Foldable
import Data.List
import Data.Ord

type Dice = (Int, Int, Int, Int, Int, Int)
--   Dice = (Top, Bottom, Left, Right, Front, Back)
type DiceState = (Int, Int, Int)

type DpKey = (Int, Int, DiceState)
type DpVal = Int
type DpArr = M.Map DpKey DpVal

diceStart :: Dice
diceStart = (1, 6, 3, 4, 2, 5)

top :: Dice -> Int
top (to, _, _, _, _, _) = to

state :: Dice -> DiceState
state (to, _, le, _, fr, _) = (to, le, fr)

fromState :: DiceState -> Dice
fromState (to, le, fr) = (to, 7-to, le, 7-le, fr, 7-fr)

rollDown :: Dice -> Dice
rollDown (to, bo, le, ri, fr, ba) = (ba, fr, le, ri, to, bo)

rollRight :: Dice -> Dice
rollRight (to, bo, le, ri, fr, ba) = (le, ri, bo, to, fr, ba)

topS :: DiceState -> Int
topS (to, _, _) = to

rollRightS :: DiceState -> DiceState
rollRightS = state . rollRight . fromState

rollDownS :: DiceState -> DiceState
rollDownS = state . rollDown . fromState

findDiceNow :: (Int, Dice) -> (Int, Dice) -> Dice
-- findDiceNow (-1, _) (_, d2) = rollRight d2
findDiceNow (_, d1) (-1, _) = rollDown d1
findDiceNow (_, _) (_, d2) = rollRight d2

mergeOrInsert :: Ord k => (v -> v -> v) -> v -> k -> M.Map k v -> M.Map k v
mergeOrInsert f dVal k dp = dp2
    where
        insertVal = maybe dVal (f dVal) (M.lookup k dp)
        dp2 = M.insert k insertVal dp

-- solveDp :: Arr2 -> Int -> Int -> (Int, Maybe Dice, Arr2)
-- solveDp dp m n | m <= 0 || n <= 0 = (-1, Nothing, dp)
-- solveDp dp 1 1 = (top diceStart, Just diceStart, M.insert (1, 1, state diceStart) (top diceStart, diceStart) dp)
-- solveDp dp m n | M.member (m, n) dp = (ansDp, Just diceDp, dp)
--     where
--         (ansDp, diceDp) = fromMaybe undefined (M.lookup (m, n) dp)
-- solveDp dp m n = ret
--     where
--         (ans1, dice1, dp1) = solveDp dp (m-1) n
--         (ans2, dice2, dp2) = solveDp dp1 m (n-1)
--         -- diceNow = findDiceNow (ans1, dice1) (ans2, dice2)
--         diceFrom1 = rollDown <$> dice1
--         diceFrom2 = rollRight <$> dice2
--         ansFrom1 = maybe (-1::Int) ((+ans1) . top) diceFrom1
--         ansFrom2 = maybe (-1::Int) ((+ans2) . top) diceFrom2
--         diceNow = fromMaybe undefined (diceFrom1 <|> diceFrom2)
--         ans = max ansFrom1 ansFrom2
--         ret = (ans, Just diceNow, M.insert (m, n) (ans, diceNow) dp2)

type NaiveState = (Int, Int, Int)
solveNaive :: NaiveState -> DiceState -> Int -> Int -> Maybe Int
solveNaive (a, mc, nc) ds m n   | mc == m && nc == n    = Just (a + topS ds)
solveNaive (_, mc, nc) _ m n    | mc > m || nc > n      = Nothing
solveNaive (a, mc, nc) ds m n = max ansFromDown ansFromRight -- uncurry max <$> ((,) <$> ansFromDown <*> ansFromRight)
    where
        ansCurr = a + topS ds
        dp = undefined
        dpUpd :: DpArr
        dpUpd = mergeOrInsert max ansCurr (mc, nc, ds) dp
        ansFromDown = solveNaive (a + topS ds, mc + 1, nc) (rollDownS ds) m n
        ansFromRight = solveNaive (a + topS ds, mc, nc + 1) (rollRightS ds) m n

type State = (DpArr, Int, Int, Int)
solveDpFill :: State -> DiceState -> Int -> Int -> DpArr
solveDpFill (dp, a, m, n) ds mUntil nUntil      | m == mUntil || n == nUntil    = dpNext
    where
        ansCurr = a + topS ds
        dpNext = mergeOrInsert max ansCurr (m, n, ds) dp
solveDpFill (dp, a, m, n) ds mUntil nUntil      | ansDp >= ansCurr              = dp
    where
        ansCurr = a + topS ds
        ansDp = fromMaybe (-1::Int) (M.lookup (m, n, ds) dp)
solveDpFill (dp, a, m, n) ds mUntil nUntil = dpNext
    where
        ansCurr = a + topS ds
        dp1 = solveDpFill (dp, ansCurr, m+1, n) (rollDownS ds) mUntil nUntil -- fromDown
        dp2 = solveDpFill (dp1, ansCurr, m, n+1) (rollRightS ds) mUntil nUntil --fromRight
        dpNext = mergeOrInsert max ansCurr (m, n, ds) dp2


tupleHead3 :: (a, b, c) -> a
tupleHead3 (a, _, _) = a

solve :: Int -> Int -> Int
-- solve m n = tupleHead3 $ solveDp M.empty m n
solve m n = fromMaybe (-1::Int) (solveNaive (0, 1, 1) (state diceStart) m n)

leaveThd :: (a, b, c) -> (a, b)
leaveThd (a, b, _) = (a, b)

solveAll :: [(Int, Int)] -> [Int]
solveAll mns = map (\(m, n) -> fromMaybe (-1::Int) $ M.lookup (m, n) dpEnd) mns
    where
        maxm = maximum (map fst mns)
        maxn = maximum (map snd mns)
        dpEmpty = M.empty
        dpFilled = solveDpFill (dpEmpty, 0, 1, 1) (state diceStart) (maxm+1) (maxn+1)
        reduced = map (\(k, v) -> (leaveThd k, v)) (M.toAscList dpFilled)
        dpEnd = foldr (\(k, v) dp -> mergeOrInsert max v k dp) M.empty reduced

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

main :: IO ()
main = do
    t <- read <$> getLine
    cases <- replicateM t getPair
    -- let anss = map (show . uncurry solve) cases
    let anss = map show $ solveAll cases
    -- [String] -> [IO ()]
    -- [IO ()] -> IO [()]
    traverse_ putStrLn anss


testInput :: [(Int, Int)]
testInput = [(i, j) | i <- [1..4], j <- [1..4]]

mapToOutput :: [(Int, Int)] -> [((Int, Int), Int)]
mapToOutput input = zip input (map (uncurry solve) input)

testOutput :: [((Int, Int), Int)]
testOutput = mapToOutput testInput