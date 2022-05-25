
leaveFst :: (a, b, c) -> (b, c)
leaveFst (_, b, c) = (b, c)

leaveSnd :: (a, b, c) -> (a, c)
leaveSnd (a, _, c) = (a, c)

leaveThd :: (a, b, c) -> (a, b)
leaveThd (a, b, _) = (a, b)

pairHead :: [a] -> (a, a)
pairHead (x1:x2:_) = (x1, x2)
pairHead _ = error "not enough items"

triHead :: [a] -> (a, a, a)
triHead (x1:x2:x3:_) = (x1, x2, x3)
triHead _ = error "not enough items"

merge2 :: (a -> b -> c) -> (a, b) -> c
merge2 = uncurry

merge3 :: (a -> b -> c -> d) -> (a, b, c) -> d
merge3 f (a, b, c) = f a b c

flip2 :: (a, b) -> (b, a)
flip2 (a, b) = (b, a)

cycle2 :: (a, b) -> (b, a)
cycle2 = flip2

cycle3 :: (a, b, c) -> (c, a, b)
cycle3 (a, b, c) = (c, a, b)