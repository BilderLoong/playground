{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Int
import Distribution.Types.AnnotatedId (AnnotatedId (AnnotatedId))
import Text.Read (Lexeme (Number))

data Example = MakeExample deriving (Show)
newtype Goats = Goats Int deriving (Eq, Show, TooMany)
type Cow = Int

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

newtype Pair = Pair (Int, String) deriving (Show, Eq)
instance TooMany Pair where
  tooMany (Pair (i, _)) = i > 42

instance TooMany (Int, Int) where
  tooMany (a, b) = (a + b) > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, b) = tooMany (a, b)

-- Or:
-- instance (Num a, TooMany a) => TooMany (a, a) where
--   tooMany (a, b) = tooMany $ a + b

-- instance (Num a, TooMany a) => TooMany (a, a) where
--   tooMany (a, b) = (a + b) > 42

-- tooMany (a, b) = (a + b) > 0

-- instance TooMany Goats where
--   tooMany (Goats n) = n > 42

-- instance TooMany Cow where
--   tooMany n = n  101

data NumberOrBool = Numba Int8 | BoolyBool Bool
  deriving (Eq, Show)

n = Numba (-128)

t = (,) 1 "2"

data QuantumBool
  = QuantumTrue
  | QuantumFalse
  | QuantumBoth
  deriving (Eq, Show)

type TwoQs = (QuantumBool, QuantumBool)

data Person'
  = MkPerson String Int
  deriving (Eq, Show)

data Person = Person
  {name :: String, age :: Int}
  deriving (Eq, Show)

data Fiction = Fiction deriving (Show)
data Nonfiction = Nonfiction deriving (Show)
data BookType
  = FictionBook Fiction
  | NonfictionBook Nonfiction
  deriving (Show)

type AuthorName = String
data Author = Author (AuthorName, BookType)

data Author' = Fiction' AuthorName | Nonfiction' AuthorName
  deriving (Eq, Show)

-- data FlowerType
--   = Gardenia
--   | Daisy
--   | Rose
--   | Lilac
--   deriving (Show)
--
-- data Garden
--   = Garden Gardener FlowerType
--   deriving (Show)

type Gardener = String

data Garden' = Gardenia Gardener | Daisy Gardener | Rose Gardener | Lilac Gardener
  deriving (Show)

data GuessWhat
  = ChickenButt
  deriving (Eq, Show)

newtype Id a
  = MkId a
  deriving (Eq, Show)
data Product a b
  = Product a b
  deriving (Eq, Show)
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)
data RecordProduct a b = RecordProduct
  { pfirst :: a
  , psecond :: b
  }
  deriving (Eq, Show)

newtype NumCow
  = NumCow Int
  deriving (Eq, Show)
newtype NumPig
  = NumPig Int
  deriving (Eq, Show)

data Farmhouse
  = Farmhouse NumCow NumPig
  deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep
  = NumSheep Int
  deriving (Eq, Show)
data BigFarmhouse
  = BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)
type BigFarmhouse' =
  Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int
data CowInfo
  = CowInfo Name Age
  deriving (Eq, Show)
data PigInfo
  = PigInfo Name Age LovesMud
  deriving (Eq, Show)
data SheepInfo
  = SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)
data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

bess' = CowInfo "bess" 4
bess = First bess' :: Animal'

e' = Second (SheepInfo "Elmer" 5 5)
elemr = Second e' :: Animal'

elmo' = Second (SheepInfo "Elmo" 5 5)
elmo = First elmo'

data Twitter
  = Twitter
  deriving (Eq, Show)

data AskFm
  = AskFm
  deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

type SN = Sum Twitter AskFm

sn = Second AskFm :: SN
sn' = First Twitter :: SN

myRecord = RecordProduct 43 2.1

myRecord' :: RecordProduct Integer Float
myRecord' = RecordProduct{pfirst = 42, psecond = 0.3}

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)
data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)
data Programmer = Programmer {os :: OperatingSystem, lang :: ProgLang}
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]
allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer o l | o <- allOperatingSystems, l <- allLanguages]
allProgrammers' :: [Programmer]
allProgrammers' = map (uncurry Programmer) [(o, l) | o <- allOperatingSystems, l <- allLanguages]

newtype Name' = Name' String deriving (Show)
newtype Acres = Acres String deriving (Show)

data FarmerType
  = DairyFarmer
  | WheatFarmer
  | SoybeanFarmer
  deriving (Show)

data Farmer
  = Farmer Name' Acres FarmerType
  deriving (Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec
  { name' :: String
  , acres :: Acres
  , farmerType :: FarmerType
  }

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of
  DairyFarmer -> True
  _ -> False
