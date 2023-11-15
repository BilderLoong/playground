module Show where

import Data.List (sort)

data Mood = Blah

instance Show Mood where
  show _ = "Blah"

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a
newtype Age
  = Age Integer
  deriving (Eq, Show)

newtype Year
  = Year Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber = Age
  toNumber (Age n) = n
  defaultNumber = Age 65

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 1988

data Person = Person Bool deriving (Show)
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood0
  = Blah0
  | Woot0
  deriving (Show, Eq)

settleDown :: Mood0 -> Mood0
settleDown x =
  if x == Woot0
    then Blah0
    else x

type Subject = String
type Verb = String
type Object = String
data Sentence = Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks
  = Rocks String
  deriving (Eq, Show)

data Yeah
  = Yeah Bool
  deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- phew = Papu "chases" True
truth =
  Papu
    (Rocks "chomskydoz")
    (Yeah True)
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p

i :: (Num a) => a
-- i :: a
i = 1

-- f0 :: (Num a) => a
-- f0 = 1.0

f :: (Fractional a) => a
f = 1.0

fc :: (Num a) => a -> a
fc = undefined
xc = fc 1.0

f2 :: (RealFrac a) => a
f2 = 1.0

freud :: (Ord a) => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int
sigmund :: Int -> Int
-- sigmund :: a -> a
sigmund x = myX

myX' = 1 :: Int
sigmund' :: Int -> Int
-- sigmund' :: (Num a) => a -> a
sigmund' x = myX

jung :: [Int] -> Int
jung = head . sort

young :: [Char] -> Char
young xs = head (sort xs)

-- young :: Ord a => [a] -> a

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- signifier :: Ord a => [a] -> a

chk :: (Eq b) => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

arith :: (Num b) => (a -> b) -> Integer -> a -> b
arith f i x = f x + fromInteger i

triple = \x -> x * 3

mTh :: (Num a) => a -> a -> a -> a
mTh = \x -> \y -> \z -> x * y * z

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
 where
  f = \n -> n + 1

addFive = \x y -> (if x > y then y else x) + 5

mflip f x y = f y x

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

newtype Username
  = Username String

newtype AccountNumber
  = AccountNumber Integer

data User
  = UnregisteredUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber num)) = putStrLn $ name ++ " " ++ show num

myUser = Username "birudo"
myAccNum = AccountNumber 514

rUser = RegisteredUser myUser myAccNum

data WherePenguinsLive
  = Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin
  = Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng place) = place

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

f3 :: (a, b) -> (c, d) -> ((b, d), (a, c))
f3 (a, b) (c, d) = ((b, d), (a, c))

snd3 (_, x, _) = x

k :: (a, b) -> a
k (x, y) = x

k2 :: String
k2 = k ("s", 1 + 2)

f0' :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f0' (a, b, c) (d, e, f) = ((a, d), (c, f))

funcZ' x = if x + 1 == 1 then "Awe" else "Wut"

funcZ x = case x + 1 == 1 of
  True -> "Awe"
  False -> "Wut"

pal xs = case xs == reverse xs of
  True -> "Yes"
  False -> "No"

pal' xs = case y of
  True -> "Yes"
  False -> "No"
 where
  y = xs == reverse xs

greetIfCool :: String -> IO ()
greetIfCool coolness = case cool of
  True -> putStr "hei hei"
  False -> putStr "pshhhh."
 where
  cool = coolness == "cool"

functionC x y = case (x > y) of
  True -> x
  False -> y

functionG x y
  | x > y = True
  | otherwise = False

ifEvenAdd2 x = case mod x 2 of
  0 -> x + 2
  _ -> x

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- fSub x y = flip (-) x y
fSub = flip (-)

data Employee
  = Coder
  | Manager
  | Veep
  | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn
    $ show e
    ++ " is the boss of "
    ++ show e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool e e' = compare e e'

employeeRank ::
  (Employee -> Employee -> Ordering) ->
  Employee ->
  Employee ->
  IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ ->
      putStrLn
        "Neither employee\
        \ is the boss"
    LT -> (flip reportBoss) e e'

dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: (Num a) => a -> a
oneIsOne = dodgy 1

oneIsTwo :: (Num a) => a -> a
oneIsTwo = flip dodgy 2

isRight x y z
  | x ^ 2 + y ^ 2 == z ^ 2 = "RIGHT!"
  | otherwise = "Wrong!"

avgGrade x
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
 where
  y = x / 100

pal''' xs
  | xs == reverse xs = True
  | otherwise = False

numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

-- Point free style
add :: Int -> Int -> Int
add x y = x + y

addPF = (+)

addOne = \x -> x + 1

addOnePF = (+ 1)

printAddOne :: IO ()
printAddOne = do
  print (0 :: Int) -- 0
  print (add 1 0) -- 1
  print (addOne 0) -- 1
  print (addOnePF 0) -- 1
  print ((addOne . addOne) 0) -- 2
  print ((addOnePF . addOne) 0) -- 2
  print ((addOne . addOnePF) 0) -- 2
  print ((addOnePF . addOnePF) 0) -- 2
  print (negate (addOne 0)) -- -1
  print ((negate . addOne) 0) -- -1
  print
    ( ( addOne -- 2
          . addOne -- 1
          . addOne -- 0
          . negate -- -1
          . addOne -- 1
      )
        0
    )

f0' :: (Ord a) => a -> a -> Bool
f0' = undefined
