myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (head:tail) = head * myProduct tail