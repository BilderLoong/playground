import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate
      ( UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)
      )
  , DbNumber 9001
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate
      ( UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123)
      )
  ]

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral (sumDb xs) / fromIntegral (length $ filterDbNumber xs)

avgDb' :: [DatabaseItem] -> Double
avgDb' di = sum / fromIntegral count
 where
  (sum, count) = foldr avgDbNumber (0, 0) di
  avgDbNumber :: DatabaseItem -> (Double, Integer) -> (Double, Integer)
  avgDbNumber (DbNumber n) (sum, count) = (sum + fromInteger n, count + 1)
  avgDbNumber _ acc = acc

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr sumDbNumber 0
 where
  sumDbNumber (DbNumber n) = (+ n)
  sumDbNumber _ = id

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr findMostRecent earlierDate
 where
  earlierDate = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)
  findMostRecent :: DatabaseItem -> UTCTime -> UTCTime
  findMostRecent (DbDate time) = max time
  findMostRecent _ = id -- return the `acc`.

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr filterNumbers []
 where
  filterNumbers (DbNumber i) acc = i : acc
  filterNumbers _ acc = acc

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr filterDates []
 where
  filterDates x acc =
    case x of
      DbDate u -> u : acc
      _ -> acc

concatFirst3Letter :: [String] -> String
concatFirst3Letter = foldr (\x acc -> take 3 x ++ acc) ""
concatFirst3Letterl :: [String] -> String
concatFirst3Letterl = foldl (\acc x -> acc ++ take 3 x) ""

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ x [] = x
foldr' f z (x : xs) = f x (foldr' f z xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ x [] = x
foldl' f z (x : xs) = foldl' f (f z x) xs

x0 = foldr (*) 1 [1 .. 5]

-- xa = flip (*) 1 [1 .. 5]
xb = foldl (flip (*)) 1 [1 .. 5]
xc = foldl (*) 1 [1 .. 5]

-- a' = foldr (++)  ["woot", "WOOT", "woot"]
a = foldr (++) "" ["woot", "WOOT", "woot"]

-- b' = foldr max [] "fear is the little death"
b = foldr max 'a' "fear is the little death"

-- c' = foldr and True [False, True]
c = foldr (&&) True [False, True]

d = foldr (||) True [False, True]

-- e' = foldl ((++) . show) "" [1 .. 5]
e = foldl (flip ((++) . show)) "" [1 .. 5]

-- f' = foldr const 'a' [1 .. 5]
f = foldr (flip const) 'a' [1 .. 5]

-- g = foldr const 0 "tacos"
g = foldr (\_ x -> x) 0 "tacos"

h = foldl const 0 "burritos"
i = foldl const 'z' [1 .. 5]

fibs' = take 20 (1 : scanl (+) 1 fibs)
fibs1 = takeWhile (< 100) (1 : scanl (+) 1 fibs)

factorial = 1 : scanl (*) 1 factorial



fibs = 1 : scanl (+) 1 fibs
fibs = 1 : scanl (+) 1 fibs 
  = 1 : scanl (+) 1 (1 : scanl (+) 1 fibs)
  = 1 : 1 : scanl (+) (1 + 1) (scanl (+) 1 fibs)
  = 1 : 1 : scanl (+) 2 (scanl (+) 1 fibs)
  = 1 : 1 : scanl (+) 2 ()

scanl (+) 1 [1,2,3] 
  == 1 : scanl (+) (1 + 1) [2,3] 
  == 1 : 2 : scanl (+) 4 [3]
  == 1 : 2 : 4 : scanl (+) 7 []
  == 1 : 2 : 4 : 7 : []
