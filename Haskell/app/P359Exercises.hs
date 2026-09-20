import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (intercalate, intersperse)

k :: (x, y) -> x
k (x, y) = x

k1 = k ((4 - 1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

f ::
  (a, b, c) ->
  (d, e, f) ->
  ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

f' :: a -> a
f' x = x

tensDigit :: (Integral a) => a -> a
tensDigit x = d
 where
  xLast = x `div` 10
  d = xLast `mod` 10

tensDigit' :: (Integral a) => a -> a
tensDigit' x = d
 where
  (xLast, _) = x `divMod` 10
  (_, d) = xLast `divMod` 10

hunsD x = d2
 where
  d = x `div` 100
  d2 = d `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x _ False = x
foldBool _ y True = y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b = case b of
  True -> y
  False -> x

foldBool0 :: a -> a -> Bool -> a
foldBool0 x y b
  | b = y
  | otherwise = x

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show
print' = do
  print (roundTrip 4 :: Int)
  print (id 4)

-- Base case return is not correct will cause the f be applied one more time.
-- applyTimes' 0 f = f
applyTimesPF 0 f = id
applyTimesPF times f = f . applyTimesPF (times - 1) f

applyTimes 0 f b = b
applyTimes n f b =
  f . applyTimes (n - 1) f $ b

fM :: Bool -> Maybe Int
fM False = Just 0
fM _ = Nothing

fibonacci :: (Integral a) => a -> a
fibonacci x
  | x <= 1 = x
-- fibonacci 0 = 0
-- fibonacci 1 = 1
fibonacci x = fibonacci (x - 2) + fibonacci (x - 1)

data DividedResult = Result Integer | DividedByZero deriving (Show)

instance Num DividedResult where
  negate (Result x) = Result $ negate x
  negate DividedByZero = DividedByZero

dividedBy :: (Integral a) => a -> a -> DividedResult
dividedBy num denom
  | denom == 0 = DividedByZero
  | num < 0 && denom > 0 = negate $ go (-num) denom 0
  | num >= 0 && denom < 0 = negate $ go num (-denom) 0
  | num < 0 && denom < 0 = go (-num) (-denom) 0
  | otherwise = go num denom 0
 where
  go n d count
    | n >= d = go (n - d) d (count + 1)
    | otherwise = Result count

dividedBy'' :: (Integral a) => a -> a -> DividedResult
dividedBy'' num denom
  | denom == 0 = DividedByZero
  | (num >= 0 && denom > 0) || (num <= 0 && denom < 0) = go (abs num) (abs denom) 0
  | otherwise = negateResult $ go (abs num) (abs denom) 0
 where
  negateResult (Result x) = Result (-x)
  go n d count
    | n >= d = go (n - d) d (count + 1)
    | otherwise = Result count

dividedBy' :: (Integral a) => a -> a -> (a, a)
dividedBy' num denom = go num denom 0
 where
  go n d count
    | n < d = (count, n)
    | otherwise =
        go (n - d) d (count + 1)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types
flippy = flip cattyConny
appedCatty = cattyConny "woops"
frappe = flippy "haha"

addFrom :: (Eq a, Num a) => a -> a
addFrom 1 = 1
addFrom n = n + addFrom (n - 1)

multiply :: (Integral a) => a -> a -> a
multiply x y = go x y 0
 where
  go _ 0 _ = 0
  go base 1 _ = base
  go base times res =
    base + go base (times - 1) (res + base)

mc91 x
  | x > 100 = x - 10
  | otherwise = 91

wordNumber :: (Integral a) => a -> String
wordNumber x = intercalate "-" numberList
 where
  numberList = map digitToWord (digits x)

digits :: (Integral a) => a -> [a]
digits x = go x []
 where
  go ds dl
    | ds == 0 = dl
    | otherwise = go (ds `div` 10) (ds `mod` 10 : dl)

digitToWord :: (Integral a) => a -> String
digitToWord x = case x of
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> "Invalid digit"

mySqr = [x ^ 2 | x <- [1 .. 19]]
myEvenSqr = [x ^ 2 | x <- [1 .. 19], rem x 2 == 0]
mySqr' = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

acro xs = [x | x <- xs, elem x ['A' .. 'Z']]

mySqr0 = [x ^ 2 | x <- [1 .. 5]]
myCube = [y ^ 3 | y <- [1 .. 5]]

myLCTuple = [(x, y) | x <- mySqr0, y <- myCube]
myLCTuple' = [(x, y) | x <- mySqr0, y <- myCube, x < 50 && y < 50]

listLength = count 0
 where
  count :: Integer -> [a] -> Integer
  count acc [] = acc
  count acc (_ : xs) = count (acc + 1) xs
