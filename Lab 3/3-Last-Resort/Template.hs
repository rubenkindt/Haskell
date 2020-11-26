module Template where

import Data.List (delete)

-- * Selection Sort
-- ----------------------------------------------------------------------------

selectionSort :: Ord a => [a] -> [a]
selectionSort = error "Not implemented"

-- * Quicksort
-- ----------------------------------------------------------------------------

partitionFold :: (a -> Bool) -> [a] -> ([a],[a])
partitionFold = error "Not implemented"

partitionFilter :: (a -> Bool) -> [a] -> ([a],[a])
partitionFilter = error "Not implemented"

partitionLC :: (a -> Bool) -> [a] -> ([a],[a])
partitionLC = error "Not implemented"

quicksort :: Ord a => [a] -> [a]
quicksort = error "Not implemented"