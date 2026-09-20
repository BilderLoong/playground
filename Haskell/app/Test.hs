sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple x = x * 3

half x = x / 2

square x = x * x

circleLength d = pi * (d * d)

-- module Main where
--
-- factorial n = if n == 0 then 1 else n * factorial (n - 1)
-- main = do
--   putStrLn "What is 5! ?"
--   x <- readLn
--   if x == factorial 5
--     then putStrLn "Right"
--     else putStrLn "Wrong"

-- main = do putStrLn "What is 2 + 2?"
--           x <- readLn
--           if x == 4
--             then putStrLn "You're right!"
--             else putStrLn "You're wrong!"
