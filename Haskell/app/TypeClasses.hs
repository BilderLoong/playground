module TypeClasses where

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek
  = Mon
  | Tue
  | Weds
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Show)

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ

data Date
  = Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

-- The Date of below line is type constructor not value constructor.
instance Eq Date where
  (==) (Date weekDay dayOfMonth) (Date weekDay' dayOfMonth') =
    weekDay == weekDay' && dayOfMonth == dayOfMonth'

data Identity a = Identity a

instance (Eq a) => Eq (Identity a) where
  -- The implemented (==) must belong to the Eq type class.
  (==) (Identity v') (Identity v) = v' == v

data NoEq = NoEqInst deriving (Show)

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  TisAn i == TisAn i' = i == i'

data TwoInteger = Two Integer Integer
instance Eq TwoInteger where
  (==) (Two x y) (Two x' y') = x' == x && y' == y

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  TisAnInt i == TisAnInt i' = i == i'
  TisAString s == TisAString s' = s == s'
  -- TisAnInt i == TisAString s = False
  _ == _ = False

data Pair a
  = Pair a a
instance (Eq a) => Eq (Pair a) where
  Pair a a0 == Pair a' a0' = a == a' && a0 == a0'

data Tuple a b = Tuple a b
  deriving (Show)
instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple a b == Tuple a' b' = a == a' && b == b'

data Which a = ThisOne a | ThatOne a
instance (Eq a) => Eq (Which a) where
  ThisOne a == ThisOne a' = a == a'
  ThatOne a == ThatOne a' = a == a'
  _ == _ = False

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello a == Hello a' = a == a'
  Goodbye a == Goodbye a' = a == a'
  _ == _ = False

divideThenAdd x y = (x / y) + 1

numId = id :: (Num a) => a -> a
intId = intId :: Int -> Int

