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
