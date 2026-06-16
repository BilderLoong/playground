import Control.Monad.State

add :: Int -> Int -> (String, Int)
add x s =
  let newS = x + s
   in ("Result: " ++ show newS ++ "; ", newS)

add' :: Int -> State Int String
add' x = state $ \s -> add x s

add'' :: Int -> State Int String
add'' x =
  get >>= \s ->
    let newS = x + s
     in put newS
          >>= \s -> return ("Result: " ++ show newS ++ "; ")

main :: IO ()
main = do
  let (log0, state0) = add 1 2
  let (log1, state1) = add 1 state0

  print $ log1 ++ "Final state" ++ show state1

-- newtype State' s a = State
--   {runState' :: s -> (a, s)}

-- -- State $ s -> (a, s) -> (a -> State $ s -> (b, s)) -> (State $ s -> (b, s))
-- stateBind :: State' s a -> (a -> State' s b) -> State' s b
-- stateBind a f = State $ \s ->
--   let (a', s') = runState' a s
--    in runState' (f a') s'
