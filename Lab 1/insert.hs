insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (n:ns) 
  | x <= n = (x:n:ns) 
  | otherwise = n : insert x ns