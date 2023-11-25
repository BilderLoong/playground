import Data.Char

cipher :: [Char] -> Int -> [Char]
cipher "" _ = ""
cipher xs n = map (`rightShift` n) xs

rightShift :: Char -> Int -> Char
rightShift c n = chr $ alphPosition2Unicode alphCharPosition
 where
  unicodeBeforeMod = ord c + n
  alphCharPosition = mod (unicodeBeforeMod - ord 'a') 26
  alphPosition2Unicode = (ord 'a' +)


-- decipher "" = ""
-- decipher xs n = 
