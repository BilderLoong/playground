{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
module Curry where

nonesense :: Bool -> Integer
nonesense True = 1
nonesense False = 2

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + nonesense b

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + nonesense b

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + nonesense b

anonymousNest :: Integer -> Bool -> Integer
anonymousNest = \i -> \b -> i + nonesense b

curry f (x, y) = f x y
uncurry f x y = f (x, y)

u = undefined

f :: a -> a -> a -> a
f = u

x :: Char = u

g :: a -> b -> c -> b
g = u

h :: (Num a, Num b) => a -> b -> b
h = u

x234 = h 1 (5.5 :: Double)

jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined
x0 = jackal "keyboard" "has the word jackal in it"

kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined
x1 = kessel 1 2

-- impossible :: a -> a
-- impossible a = a + a

hypothetical :: a -> a -> a
hypothetical a b = a

hypothetical0 :: a -> a -> a
hypothetical0 a b = b

parametricity :: a -> b -> b
parametricity x y = y

xs = [1, 2, 3]
xsRes = 6 / fromIntegral (length xs)
