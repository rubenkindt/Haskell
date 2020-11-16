foldr' :: (Int -> a -> a) -> a -> [Int] -> a
foldr' f z []       = z
foldr' f z (x : xs) = f x (foldr f z xs)


filter' :: [Int] -> (Int -> Bool) -> [Int]
filter' list p = foldr' (\x y -> if p x then x : y  else y ) [] list

--filter' list f = foldr' (\x y -> (helper f x y)) [] list

{-
helper :: (Int -> Bool) -> Int -> [Int] -> [Int]
helper f x y
 if f x then x:y else y
 
 
filter'' [Int] -> (Int -> Bool) -> [Int]
filter'' [] p = []
filter'' (x:xs) p = if p x then x : filter'' p xs else filter'' p xs
-}



