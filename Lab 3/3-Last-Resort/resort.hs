module Template where

import Data.List (delete)

-- * Selection Sort
-- ----------------------------------------------------------------------------

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort (x:xs) = small:selectionSort (delete small (x:xs)) 
 where small = smallest x xs
-- can probl be done with map, filter or foldr

smallest :: Ord a => a -> [a] -> a
smallest small [] = small
smallest small (x:xs) 
 | small > x = smallest x xs 
 | otherwise = smallest small xs

-- * Quicksort
-- ----------------------------------------------------------------------------

partitionFold :: (a -> Bool) -> [a] -> ([a],[a])
partitionFold f list = (foldr (\ x xs -> if f x then x:xs else xs) [] list, foldr (\ x xs -> if not(f x) then x:xs else xs) [] list) 

partitionFilter :: (a -> Bool) -> [a] -> ([a],[a])
partitionFilter f list= (filter f list, filter (not . f) list) 

partitionLC :: (a -> Bool) -> [a] -> ([a],[a])
partitionLC f list = ([r | r<- list, f r],[r |r<-list, f r]) 
-- more clear to me 
-- partitionLC f list = ([r | r<- list, f r==True],[r |r<-list, f r==False]) 


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort small ++ [x] ++ quicksort big
 where (small,big) = partitionFold (x>=) xs