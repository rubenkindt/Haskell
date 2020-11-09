add :: [int] -> Int
add list
  | null list =0
  | otherwise = head list + add (tail list)