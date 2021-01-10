
module Template where

-- * Folds
-- ----------------------------------------------------------------------------

mySum :: [Integer] -> Integer
mySum []     = 0
mySum (x:xs) = x + mySum xs

-- added By Ruben
mySum2 :: [Integer] -> Integer
mySum2 list = foldr ( \x xs-> (+) x xs) 0 list

myProduct :: [Integer] -> Integer
myProduct []     = 1
myProduct (x:xs) = x * myProduct xs

foldInts :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldInts = myFoldr -- see below

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _f base []     = base
myFoldl  f base (x:xs) = myFoldl f (f base x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _f base []     = base
myFoldr  f base (x:xs) = f x (myFoldr f base xs)

readInBase :: Int -> [Int] -> Int
readInBase nrbase digits = myFoldl (\acc d -> acc*nrbase + d) 0 digits


--(\acc d -> acc*base + d) 0 FirstDigit        -> 0*base+FirstDigit 
--(\acc d -> acc*base + d) FirstDigit SecDigit -> FirstDigit*base+SecDigit 


myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

myMapF :: (a -> b) -> [a] -> [b]
myMapF f xs = myFoldr (\x -> (f x :)) [] xs

