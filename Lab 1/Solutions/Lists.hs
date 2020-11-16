
module Template where

-- * List Operations
-- ----------------------------------------------------------------------------

myProduct :: [Integer] -> Integer
myProduct []     = 1
myProduct (x:xs) = x * myProduct xs

insert :: Int -> [Int] -> [Int]
insert x []     = [x]
insert x (y:ys) = if x < y
                    then x : y : ys
                    else y : insert x ys

myLast :: [Int] -> Int
myLast []    = error "empty list"
myLast [x]   = x
myLast (_:t) = myLast t

