
module Template where

-- * Folds
-- ----------------------------------------------------------------------------

mySum :: [Integer] -> Integer
mySum list = error "Not implemented"

myProduct :: [Integer] -> Integer
myProduct list = error "Not implemented"

foldInts :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldInts fn base ints = error "Not implemented"

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl fn base list = error "Not implemented"

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr fn base list = error "Not implemented"

readInBase :: Int -> [Int] -> Int
readInBase base digits = error "Not implemented"

myMap :: (a -> b) -> [a] -> [b]
myMap fn list = error "Not implemented"

myMapF :: (a -> b) -> [a] -> [b]
myMapF fn list = error "Not implemented"

