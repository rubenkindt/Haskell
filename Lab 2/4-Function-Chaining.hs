
module Template where

-- * Function Chaining
-- ----------------------------------------------------------------------------

applyAll :: [a -> a] -> a -> a
applyAll [] a = a
applyAll (x:xs) a = x $ applyAll xs a


applyTimes :: Int -> (a -> a) -> a -> a
applyTimes 0 f b= b
applyTimes a f b = applyTimes (a-1) f $ f

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs = error "Not implemented"

