myLast :: [Int] -> Int
myLast (n:ns) 
  | ns==[] =n
  | otherwise = myLast ns