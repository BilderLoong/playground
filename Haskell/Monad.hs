import Control.Monad

a ::  Maybe Double
a = Nothing

sumNumbers :: IO ()
sumNumbers =
  putStrLn "Enter the first number:"
    >> readLn
    >>= \num1 ->
      putStrLn "Enter the second number:"
        >> readLn
        >>= \num2 ->
          let sum = num1 + num2 
           in putStrLn ("The sum is: " ++ show sum)

main :: IO ()
main = sumNumbers
