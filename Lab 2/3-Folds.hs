
module Template where

-- * Folds
-- ----------------------------------------------------------------------------

mySum :: [Integer] -> Integer
mySum [] = 0 
mySum (i:is) = i + mySum is 
--mySum list = foldr (+) 0 list

myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (m:ms) = m * myProduct ms
--myProduct list = foldr (*) 1 list

foldInts :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldInts fn base [] = base
foldInts fn base (x:xs) = fn x (foldInts fn base xs)


myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl fn basec [] = basec
myFoldl fn basec (x:xs) = myFoldl fn (fn basec x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr fn basec [] = basec
myFoldr fn basec (x:xs) = fn x (myFoldr fn basec xs)

readInBase :: Int -> [Int] -> Int
readInBase base list = myFoldl (\a b ->  b + base * a ) 0 list 

{-
only works reverce
readInBase :: Int -> [Int] -> Int
readInBase base (x:[]) = x
readInBase base (x:xs) = x + base * readInBase base xs 
-}

-- attempt made, but failed
readInBase' :: Int -> [Int] -> Int
readInBase' base (x:[]) = x + base
readInBase' base (x:xs) = readInBase' (base * x) xs 


myMap :: (a -> b) -> [a] -> [b]
myMap fn [] = []
myMap fn (x:xs) = fn x : myMap fn xs 


myMapF :: (a -> b) -> [a] -> [b]
myMapF fn list = foldr (\x y -> fn x : y) [] list

