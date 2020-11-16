foldr' :: (Int -> a -> a) -> a -> [Int] -> a
foldr' f z []       = z
foldr' f z (x : xs) = f x (foldr f z xs)




length' :: [Int] -> Int
length' list = foldr' (\_ x -> x+1) 0 list

any' :: (Int -> Bool) -> [Int] -> Bool
any' f list = foldr' (\x y-> (f x ) || y) False list

all' :: (Int -> Bool) -> [Int] -> Bool
all' f list = foldr' (\x y-> (f x ) && y) True list

map' :: (Int -> Int) -> [Int] -> [Int]
map' f list = foldr' (\x y -> f x : y) [] list

filter' :: [Int] -> (Int -> Bool) -> [Int]
filter' list p = foldr' (\x y -> if p x then x : y  else y ) [] list

{-
--filter' list f = foldr' (\x y -> (helper f x y)) [] list
helper :: (Int -> Bool) -> Int -> [Int] -> [Int]
helper f x y
 if f x then x:y else y
 
 
filter'' [Int] -> (Int -> Bool) -> [Int]
filter'' [] p = []
filter'' (x:xs) p = if p x then x : filter'' p xs else filter'' p xs
-}

 
{-
insert2 :: Int -> [Int] -> [Int]
insert2 n [] = [n]
insert2 n (m:ms) 
 | n>m = m: insert2 n ms
 | otherwise = n : m:ms

wordt

insert' :: Int -> [Int] -> [Int]
-- fout: insert' n list=  foldr' (\x tail -> if n>x then x:tail else n:x:tail) [n] list
insert' n list=  foldr' (\m ms -> if n>m then m:ms else n:m: tail ms) [n] list

insert n list=  foldr' (\x tail -> if n>m then m:tail else n:m:tail) (\n [] -> [n])

-}
insert' :: Int -> [Int] -> [Int]
-- fout: insert' n list=  foldr' (\x tail -> if n>x then x:tail else n:x:tail) [n] list
insert' n list =  foldr' (\m ms -> if n>m then m:ms else n:m: tail ms) ([n]) list



even' :: Int -> Bool
even' x
 | mod x 2==0 =True
 | otherwise =False
 
not' :: Bool -> Bool
not' True =False
not' False =True

absolute' :: Int -> Int
absolute' x
 | x<0 =(-x)
 |otherwise =x

greaterThanFive :: Int -> Bool
greaterThanFive x
 |x>5 = True
 |otherwise =False

--amountEven :: [Int] -> Int
--amountEven list =foldr' (\ x y -> even' x : y ) 0 list


