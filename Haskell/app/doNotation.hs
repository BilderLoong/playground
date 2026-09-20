#!/usr/bin/env cabal
{- cabal:
build-depends: base, random, mtl
-}

import Control.Monad.State
import System.Random

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

helloThere :: IO ()
helloThere = putStrLn "hello"

rollDie :: State StdGen Die
rollDie = state $ \s ->
  let (a, s') = randomR (1, 6) s
   in (intToDie a, s')

rollDie' :: State StdGen Die
rollDie' =
  state (randomR (1, 6))
    >>= \n -> pure (intToDie n)

rollDie'' :: State StdGen Die
rollDie'' = intToDie <$> state (randomR (1, 6))

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use 'error'
    -- _extremely_ sparingly.
    x ->
      error $ "intToDie got non 1-6 integer: " ++ show x

main :: IO ()
main = helloThere