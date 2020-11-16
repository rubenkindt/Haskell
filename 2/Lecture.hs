add :: [Int] -> Int
add []     = 0
add (n:ns) = n + add ns

{-
add list
  | null list  = 0
  | otherwise  = head list + add (tail list)
-}

{-
   
   count [2,3,4,5] = 4
   count [True,False] = 2
  
 -}

count :: [a] -> Int
count []     = 0
count (x:xs) = 1 + count xs

average :: [Int] -> Int
average []   = 0
average list = add list `div` count list

addAndCount :: [Int] -> (Int , Int)
{-
addAndCount []     = (0, 0)
addAndCount (n:ns) = (n, 1) `addTuple` addAndCount ns
  where
    addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
 -}
addAndCount (n:ns) = (n + s, 1 + c)
  where
    (s,c) = addAndCount ns
    hello = "hello"


{-

   Insertion sort

   Input         Output
   [2,1,5,3]     []
   [1,5,3]       [2]
   [5,3]         [1,2]
   [3]           [1,2,5]
   []            [1,2,3,5]

 -}

{-

     insert 3 [1,5] = [1,3,5]

 -}

tuple :: a -> [b] -> [(a,b)]
tuple x []     = []
tuple x (y:ys) = (x,y) : tuple x ys


{-
     [] = n
 
     /\

     (x,y) : tuple x ys = c y (tuple x ys) 
-}

tuple' :: a -> [b] -> [(a,b)]
{-
tuple' x ys = foldr c n ys
  where
    c = \y xys -> (x,y) : xys
    n = []
-}
tuple' x ys = foldr (\y xys -> (x,y) : xys) [] ys

tuple2 :: [b] -> a -> [(a,b)]
tuple2 []     = \x -> []
tuple2 (y:ys) = \x -> (x,y) : tuple2 ys x

{-
uitleg van hieronder 
normaal neemt tuple2' 2 inputs in een [v] en een x
maar nu neemt die 1 input in en geeft het een functie (as) terug
de hunctie heeft de parameter x nodig
-}

tuple2' :: [v] -> (u -> [(u,v)])
   -- a = v
   -- b = (u -> [(u,v)])
tuple2' ys = foldr c n ys
  where

    c :: v -> (u -> [(u,v)]) -> u -> [(u,v)]
    c a as x = (x,a) : as x
      -- a  :: v
      -- as :: (u -> [(u,v)])
      -- x  :: u

    n :: u -> [(u,v)]
    n x = []  -- x :: u

{-

     \x -> [] = n
 
     /\

     \x -> (x,y) : tuple2 ys x = (\a as -> \x -> (x,a) : as x) y (tuple2 ys) 

-}


{-

  tuple' True [1,2,3]

  >->

 foldr c n [1,2,3]
  where
    c = \y xys -> (True,y) : xys
    n = []

  

 -}



insert :: Int -> [Int] -> [Int]
insert n []      = [n]
insert n (m:ms)
  = if n > m then m : insert n ms else n : m : ms


{-
     [n] = nil
 
     /\

      cons m inm = if n > m then m : inm else n : m : ms
    
-}

insert' :: Int -> [Int] -> [Int]
insert' x l = foldr cons nil l
   where
       cons :: Int -> [Int] -> [Int]
       cons m inm = if x > m then m : inm else x : m : tail inm
       nil = [x] 

para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para c n []     = n
para c n (x:xs) = c x xs (para c n xs)

insert'' :: Int -> [Int] -> [Int]
insert'' x l = para cons nil l
   where
       cons :: Int -> [Int] -> [Int] -> [Int]
       cons m ms inm = if x > m then m : inm else x : m : ms
       nil = [x] 

  -- only the recursion comes from foldr

-- insert' l = [l| l <- list, l < n] ++ [n] ++ [h| h <- list, h >= n]

insertionSort :: [Int] -> [Int]
insertionSort []     = []
insertionSort (n:ns) = insert n (insertionSort ns)

{-

  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr c n []     = n
  foldr c n (x:xs) = c x (foldr c n xs)

  Universal Property of Fold

     h = foldr c n   (we say that h is catamorphism)
    
     <=>

     h [] = n
 
     /\

     h (x:xs) = c x (h xs) 


   para :: (a -> [a] -> b -> b) -> b -> [a] -> b
   para c n []     = n
   para c n (x:xs) = c x xs (para c n xs)

-}

insertionSort' :: [Int] -> [Int]
  -- a = Int
  -- b = [Int]
insertionSort' l = foldr c n l
  where
   c :: Int -> [Int] -> [Int]
   c = insert 
   n :: [Int]
   n = []


-- idList [1,2,3] = [1,2,3]
idList :: [a] -> [a]
idList l = foldr c n l
  where
    c = (:)
    n = [] 

{-
     [] = n
 
     /\

     (x:xs) = c x xs 
-}


-- myTails []      = [[]]
-- myTails [1,2,3] = [[1,2,3],[2,3],[3],[]]

myTails :: [a] -> [[a]]
myTails l = foldr c n l
  where 
    c :: a -> [[a]] -> [[a]] 
    c x rxs = (x : head rxs) : rxs 
    n = [[]]

{-
  
    map :: (a -> b) -> ([a] -> [b])
    map f []     = []
    map f (x:xs) = f x : map f xs

 -}

-- maxTail [1,-10,2,3] =
{-
     []     0
     [3]    3
     [2,3]  5 <<<
     [-10,2,3] -5
     [1,-10,2,3] -4
-}
maxTail :: [Int] -> Int
maxTail = maximum . map sum . myTails
{-
  let 

      list_of_tails :: [[Int]]
      list_of_tails = myTails l 

      list_of_sums_of_tails :: [Int]
      list_of_sums_of_tails = map sum list_of_tails

  in maximum list_of_sums_of_tails
 -}
  

-- f [[1,2],[3,4]] = [[False,True],[False,True]]
f :: [[Int]] -> [[Bool]]
f = undefined
  -- use
  -- map
  -- even 


  -- myTails :: [a] -> [[a]]
  -- maximum :: [Int] -> Int
  -- sum     :: [Int] -> Int
  -- map

{-

     [[]] = n
 
     /\

     myTail (x:xs) = c x (myTail xs) 

    [[1,2,3],[2,3],[3],[]] = c 1 [[2,3],[3],[]]

-}

{-

   Merge sort -   split - recursively sort - merge

   [2,1,5,3]

   [2,1]   [5,3]

   [2] [1] [5] [3]
   
   [2] [1] [5] [3]
   
   [1,2]   [3,5]

   [1,2,3,5]

 -}

split :: [Int] -> ([Int], [Int])
split []     = ([], []) 
split (x:xs) = (zs, x:ys)
  where
    (ys,zs) = split xs

-- merge [1,3] [2,4] = [1,2,3,4]
merge :: [Int] -> [Int] -> [Int]
merge [] list    = list
merge list []    = list
merge (x1:xs1) (x2:xs2)
 | x1 > x2       = x2 : merge (x1:xs1) xs2
 | otherwise     = x1 : merge xs1 (x2:xs2)

mergeSort :: [Int] -> [Int]
mergeSort xs = 
  merge (mergeSort ys) (mergeSort zs)
    where (ys,zs) = split xs


