
module Template where

-- * List Comprehensions
-- ----------------------------------------------------------------------------

mapLC :: (a -> b) -> [a] -> [b]
mapLC f xs = [ f x | x <- xs]

-- added by Ruben
mapLC2 :: (a -> b) -> [a] -> [b]
mapLC2 = map 

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC p xs = [ x | x <- xs, p x ]

