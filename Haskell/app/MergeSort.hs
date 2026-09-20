mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

merge :: (Ord a) => [a]  -> [a] -> [a]
merge [] right = right
merge left [] = left
merge (l : ls) (r : rs) = case compare l r of
  GT -> r : merge (l : ls) rs
  LT -> l : merge ls (r : rs)
  _ -> l : r : merge ls rs

main :: IO ()
main = do
  print (mergeSort [12, 3, 1, 8, 3])