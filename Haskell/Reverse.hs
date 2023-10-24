module Reverse where

main :: IO ()
main = print $ rvrs "Curry is awesome!"

rvrs :: [Char] -> [Char]
rvrs x = awesome ++ is ++ curry ++ exam
 where
  awesome =
    let after = drop 9 x
     in take 7 after
  exam = [x !! 16]
  is = take 4 (drop 5 x)
  curry = take 5 x
