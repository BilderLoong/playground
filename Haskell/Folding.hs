x0 = foldr (*) 1 [1 .. 5]
-- xa = flip (*) 1 [1 .. 5]
xb = foldl (flip (*)) 1 [1 .. 5]
xc = foldl (*) 1 [1 .. 5]
