myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (_ : xs) = Just xs

mySafeHead :: [a] -> Maybe a
mySafeHead [] = Nothing
mySafeHead (x : _) = Just x

eftBool :: Bool -> Bool -> [Bool]
eftBool True True = [True]
eftBool from to
  | from > to = []
  | otherwise = from : eftBool (succ from) to

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd GT GT = [GT]
eftOrd from to
  | from > to = []
  | otherwise = from : eftOrd (succ from) to

eftInt :: Int -> Int -> [Int]
eftInt from to
  | from > to = []
  | otherwise = from : eftInt (succ from) to

eftInt' :: Int -> Int -> [Int]
eftInt' from to = go to []
 where
  go t res
    | t < from = res
    | otherwise = go (t - 1) (t : res)

eftChar :: Char -> Char -> [Char]
eftChar from to
  | from > to = []
  | otherwise = from : eftChar (succ from) to

myWord's :: String -> [String]
myWord's "" = []
myWord's str = word : myWord's rest
 where
  word = takeWhile (/= ' ') str
  rest = dropWhile (== ' ') . dropWhile (/= ' ') $ str

myWords :: String -> [String]
myWords "" = [] -- Base case: empty string returns an empty list
myWords str
  | noLeadingSpaces == "" = [] -- Termination condition: if the remaining string is empty, return an empty list
  | otherwise = word : myWords rest
 where
  -- Drop leading spaces
  noLeadingSpaces = dropWhile (== ' ') str
  -- Take characters until the next space
  word = takeWhile (/= ' ') noLeadingSpaces
  -- Drop the word and the trailing spaces, if any
  rest = dropWhile (== ' ') . dropWhile (/= ' ') $ noLeadingSpaces

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen =
  "Could frame thy fearful\
  \ symmetry?"
sentences =
  firstSen
    ++ secondSen
    ++ thirdSen
    ++ fourthSen
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

myLines :: String -> [String]
myLines str
  | noLeadingLN == "" = []
  | otherwise = line : myLines rest
 where
  noLeadingLN = dropWhile (== '\n') str
  line = takeWhile (/= '\n') noLeadingLN
  rest = dropWhile (/= '\n') noLeadingLN

test0 = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

mySplit :: [Char] -> Char -> [String]
mySplit str del
  | noLeadingDelStr == "" = []
  | otherwise = item : mySplit restStr del
 where
  noLeadingDelStr = dropWhile (== del) str
  item = takeWhile (/= del) noLeadingDelStr
  restStr = dropWhile (/= del) noLeadingDelStr
