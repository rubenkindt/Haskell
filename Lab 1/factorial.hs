{-
factorial :: Integer -> Integer
factorial 0 = 1
factorial s = s * factorial (s-1)
-}


factorial :: Integer -> Integer
factorial as 
 |as>0 = product ([ a | a <- [1..as]])
 | otherwise =0