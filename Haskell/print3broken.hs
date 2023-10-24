module Print3Broken where

printSecond :: IO ()
printSecond = do
  putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond

greeting = "Yarrrrr"

letterIndex :: Int -> Char
letterIndex x = "Curry" !! x

