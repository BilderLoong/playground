{-# LANGUAGE UndecidableInstances #-}

import Data.Monoid

anyBool = All True <> All True

x = First Nothing <> First (Just 2)

newtype Sum' a = Sum' a deriving (Show)

instance (Num a) => Semigroup (Sum' a) where
  Sum' a <> (Sum' b) = Sum' (a + b)

instance (Num a) => Monoid (Sum' a) where
  mempty = Sum' 0
