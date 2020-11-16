
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
myFoldl fn base [] = base
myFoldl fn base (x:xs) = fn (myFoldl fn base xs) x

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr fn base [] = base
myFoldr fn base (x:xs) = fn x (myFoldr fn base xs)

readInBase :: Int -> [Int] -> Int
readInBase base (x:[]) = x
readInBase base (x:xs) = x +base * readInBase base xs 

myMap :: (a -> b) -> [a] -> [b]
myMap fn [] = []
myMap fn (x:xs) = fn x : myMap fn xs 


myMapF :: (a -> b) -> [a] -> [b]
myMapF fn list = foldr (\x y -> fn x : y) [] list

