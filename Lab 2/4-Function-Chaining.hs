
module Template where

-- * Function Chaining
-- ----------------------------------------------------------------------------

applyAll :: [a -> a] -> a -> a
applyAll [] a = a
applyAll (x:xs) a = x $ applyAll xs a

-- id is de indentity function geeft gewoon terug wat zijn input is

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes 0 f = id
applyTimes a f = applyTimes (a-1) f . f

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs = error "Not implemented"

