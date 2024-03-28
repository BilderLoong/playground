{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Int
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
