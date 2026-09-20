init' :: [a] -> [a] -- init [1,2,3] = [1,2]
init' [_] = []
init' (x : xs) = x : init xs
