module TypeExcerises where

functionH (x : _) = x

functionC x y = if x > y then True else False

functionS (x, y) = y

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r x = take 0 x

co :: (b -> c) -> (a -> b) -> a -> c
co = \f1 f2 x -> f1 $ f2 x

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' f = f
