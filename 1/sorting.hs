-- insertion sort
{-
input  		output
[2,1,5,3] 	[]
[1,5,3]   	[2]
[5,3]   	[1,2]
[3]   		[1,2,5]
[]			[1,2,3,5]
-}

{-
 insert 3 [1,5] = [1,3,5]
-}
insert :: Int -> [Int] -> [Int]
insert nr []     = [nr]
insert nr (n:ns)
  |nr > n    = n: insert nr ns
  |otherwise = nr : (n : ns)


insertionSort :: [Int] -> [Int]
insertionSort []     = []
insertionSort (n:ns) = insert n (insertionSort(ns))
 
{-
merge = repearing splitting , recursive sort, merge
[2,1,5,3]
splited
[2,1]  [5,3]
[2][1]  [5][3]
[2][1]  [5][3]
[1,2]  [3,5]
[1,2,3,5]
-}


split :: [Int] -> ([Int],[Int])
split [] = ([],[])
split (x:xs) = (zs,x:ys)
  where
    (ys,zs) = split xs  
-- ^positio switched

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] list = list
merge list [] = list
merge (head1:tail1) (head2:tail2)
  | head1 > head2 = head2 : (merge (head1:tail1) tail2)
  | otherwise     = head1 : (merge tail1 (head2:tail2))
