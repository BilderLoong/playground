x0 = foldr (*) 1 [1 .. 5]

-- xa = flip (*) 1 [1 .. 5]
xb = foldl (flip (*)) 1 [1 .. 5]
xc = foldl (*) 1 [1 .. 5]

-- a' = foldr (++)  ["woot", "WOOT", "woot"]
a = foldr (++) "" ["woot", "WOOT", "woot"]

-- b' = foldr max [] "fear is the little death"
b = foldr max 'a' "fear is the little death"

-- c' = foldr and True [False, True]
c = foldr (&&) True [False, True]

d = foldr (||) True [False, True]

-- e' = foldl ((++) . show) "" [1 .. 5]
e = foldl (flip ((++) . show)) "" [1 .. 5]

-- f' = foldr const 'a' [1 .. 5]
f = foldr (flip const) 'a' [1 .. 5]

-- g = foldr const 0 "tacos"
g = foldr (\_ x -> x) 0 "tacos"

h = foldl const 0 "burritos"
i = foldl const 'z' [1 .. 5]
