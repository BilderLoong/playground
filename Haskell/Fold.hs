foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ x [] = x
foldr' f z (x : xs) = f x (foldr' f z xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ x [] = x
foldl' f z (x : xs) = foldl' f (f z x) xs

foldl' f (f (f z x) x) (x : xs) = foldl' f (f (f (f (f z x) x)x) x) xs
