{-# LANGUAGE NoMonomorphismRestriction #-}

module TypeInference1 where

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

f :: (Num a) => a -> a -> a
f x y = x + y + 3

f0 x y = x + y + 3

myConcat x = x ++ " yo"
myMult x = (x / 3) * 5
myTake x = take x "hey you"
myCom x = x > (length [1 .. 10])
myAlph x = x < 'z'

triple x = x * 3 :: Integer

triple0 x = tripleItYo x
 where
  tripleItYo :: Integer -> Integer
  tripleItYo x = x * 3

x = 1

x4 = (* 9) 6
x0 = head [(0, "doge"), (1, "kitteh")]
x1 = head [(0 :: Integer, "doge"), (1, "kitteh")]
x2 = if False then True else False
x3 = length [1, 2, 3, 4, 5]
x5 = (length [1, 2, 3, 4]) > (length "TACOCAT")

x6 = 5
y = x + 4
w = y * 10

x7 = 5
y7 = x + 5
z y = y * 10

x8 = 5
y8 = x + 5
f8 = 4 / y8

x9 = "Julie"
y9 = " <3 "
z9 = "Haskell"
f9 = x9 ++ y9 ++ z9

bigNum = (^) 5 $ 10

-- wahoo = bigNum $ 10

xx = print
yy = print "woohoo!"
zz = xx "hello world"

aa = (+)
ba = 5
ca = ba 10
da = ca 200
