#!/usr/bin/env cabal
{- cabal:
build-depends: base, random, mtl
-}

import Control.Monad.State
import System.Random

-- A single action that rolls a 6-sided die and updates the state
rollDie :: State StdGen Int
rollDie = state $ randomR (1, 6)

main :: IO ()
main = do
  let s = mkStdGen 23434
  -- runState executes the monad and returns both the result and final generator
  let (result, finalState) =
        runState
          ( do
              d1 <- rollDie
              d2 <- rollDie
              d3 <- rollDie
              return (d1, d2, d3)
          )
          s
  print result -- Output: (6, 6, 4) (or whatever seed 0 produces)