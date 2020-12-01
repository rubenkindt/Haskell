module Template where

import Data.List (delete)

-- * Selection Sort
-- ----------------------------------------------------------------------------

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort l  =
  let smallest = minimum l
  in smallest : selectionSort (delete smallest l)

-- * Quicksort
-- ----------------------------------------------------------------------------

-- Partition using a fold
partitionFold :: (a -> Bool) -> [a] -> ([a],[a])
partitionFold p = foldr go ([],[]) where
  go x (left,right)
    | p x = (x:left,right)
    | otherwise = (left,x:right)

-- Partition using a filter
partitionFilter :: (a -> Bool) -> [a] -> ([a],[a])
partitionFilter p l = (filter p l, filter (not . p) l)

-- Partition using a list comprehension
partitionLC :: (a -> Bool) -> [a] -> ([a],[a])
partitionLC p l = ([x | x <- l, p x],[x | x <- l, not (p x)])

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (p:xs) = quicksort left ++ [p] ++ quicksort right where
  (left,right) = partitionFold (<= p) xs
