module TupleAndList where

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- Eq is a type class
-- (Eq a) => in a type signature, you are indicating that the function or expression can only work with types that are instances of the Eq type class.
-- https://poe.com/s/hOOYShcyXstnkJOTKemB
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then -x else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

x = (+)

f1 xs = x w 1
 where
  w = length xs

f2 :: (a, b) -> a
f2 (a, b) = a

f3 x = x
