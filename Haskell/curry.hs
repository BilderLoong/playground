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
