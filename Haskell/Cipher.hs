import Data.Char

cipher :: [Char] -> Int -> [Char]
cipher "" _ = ""
cipher xs n = map (`loopShiftChar` n) xs

-- positiave `n`: right shift
-- negative `n`: left shift
loopShiftChar :: Char -> Int -> Char
loopShiftChar c n = chr $ alphPosition2Unicode alphCharPosition
 where
  unicodeBeforeMod = ord c + n
  alphCharPosition = mod (unicodeBeforeMod - ord 'a') 26
  alphPosition2Unicode = (ord 'a' +)

decipher "" _ = ""
decipher xs n = cipher xs (-n)

cipher' "" _ = ""
cipher' xs 0 = xs
cipher' xs n = cipher (map shiftCharForwards xs) (n - 1)

-- Also I can use `succ` or `pred` to implement shift.
shiftCharForwards :: Char -> Char
shiftCharForwards 'z' = 'a'
shiftCharForwards x = succ x

shiftCharBackwards :: Char -> Char
shiftCharBackwards 'z' = 'a'
shiftCharBackwards x = pred x

myOr :: [Bool] -> Bool
myOr [] = False
myOr (b : bs) = b || myOr bs

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) = f x || myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = myOr . map f

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem e (cur : es) = e == cur || myElem e es

myElem' :: (Eq a) => a -> [a] -> Bool
myElem' _ [] = False
myElem' e es = myAny (e ==) es

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squish' :: [[a]] -> [a]
squish' = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f xs = foldr1 (\x acc -> if f x acc == GT then x else acc) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f xs = foldr1 (\x acc -> if f x acc == LT then x else acc) xs

myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' _ [x] = x
myMaximumBy' f (x : xs) = if f x acc == GT then x else acc
 where
  acc = myMaximumBy f xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

