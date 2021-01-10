
module Template where

-- Section 4: Function Chaining
-- ----------------------------------------------------------------------------

applyAll :: [a -> a] -> a -> a
applyAll fs x = foldr ($) x fs 
-- same as: foldr (\f x -> f x) x fs
-- Alternatively, with primitive recursion:
--   applyAll []     x = x
--   applyAll (f:fs) x = f (applyAll fs x)

--myFoldr :: (a -> b -> b) -> b -> [a] -> b
--myFoldr _f base []     = base
--myFoldr  f base (x:xs) = f x (myFoldr f base xs)

-- 10/01/2021 a foldl would be more logic here
--   applyAll []     x = x
--   applyAll (f:fs) x = applyAll fs (f x)
--myFoldl :: (b -> a -> b) -> b -> [a] -> b
--myFoldl _f base []     = base
--myFoldl  f base (x:xs) = myFoldl f (f base x) xs

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes n f = applyAll (replicate n f)

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs x fs = map ($x) fs
  -- same as: map (\f -> f x) fs

