add2 :: Int -> Int -> Int
add2 int1 int2 = int1 + int2

--inc :: Int -> Int -> Int
add3 = add2 2 

sub2 :: Int -> Int -> Int
sub2 int1 int2 = int1 - int2 

add4 = sub2 1 --"add4 5" creeert sub2 1 5

inc2 = (*) 2

data Tree a = Empty | Branch a (Tree a) (Tree a)
foldTree f z Empty=z
foldTree f z(Branch x l r) =f x (foldTree f z l) (foldTree f z r)