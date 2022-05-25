-- https://www.hackerrank.com/challenges/swap-nodes?isFullScreen=true

data BiTree a = Nil | Node a (BiTree a) (BiTree a)

instance (Show a) => Show (BiTree a) where
    show Nil = ""
    show b = init $ showR b
        where
            showR Nil = ""
            showR (Node a l r) = showR l ++ show a ++ " " ++ showR r

t = Node 1 (Node 2 Nil Nil ) (Node 3 Nil Nil)

depth :: BiTree a -> Int
depth = depthR 1
    where
        depthR :: Int -> BiTree a -> Int
        depthR cd Nil = cd-1
        depthR cd (Node _ l r) = max (depthR (cd+1) l) (depthR (cd+1) r)

swapSub :: BiTree a -> BiTree a
swapSub Nil = Nil
swapSub (Node a l r) = Node a r l

swapSubAllK :: Int -> Int -> BiTree a -> BiTree a
swapSubAllK _ _ Nil = Nil
swapSubAllK d k (Node a l r) | d `mod` k == 0 = Node a (swapSubAllK k (d+1) r) (swapSubAllK k (d+1) l)
swapSubAllK d k (Node a l r) = Node a (swapSubAllK k (d+1) l) (swapSubAllK k (d+1) r)

swapSubAtK :: Int -> Int -> BiTree a -> BiTree a
swapSubAtK d k (Node a l r) | d == k    = Node a (swapSubAtK k (d+1) r) (swapSubAtK k (d+1) l)
swapSubAtK d k (Node a l r) | d < k     = Node a (swapSubAtK k (d+1) l) (swapSubAtK k (d+1) r)
swapSubAtK _ _ t = t

solve :: Show a => Int -> BiTree a -> [String]
solve k0 t0 = snd $ foldl f (t0, []) [k0, 2*k0..d]
    where
        d = depth t0
        tnx = swapSubAtK 1
        f (t, ss) k = (tnx k t, ss ++ [show (tnx k t) ++ "\n"])

constBiTreeInt :: [String] -> BiTree Int
constBiTreeInt = undefined

main :: IO ()
main = interact $ concat . solve 5 . constBiTreeInt . words