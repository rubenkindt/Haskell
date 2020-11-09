append :: [Int] -> [Int] -> [Int]
append [] lijst = lijst
append lijst [] = lijst
append (head1:tail1) (head2:tail2) = (head1: append tail1 (head2:tail2))
  
{-
append (head1:tail1) (head2:tail2) = (head1:xxx)
  where xxx = append tail1 (head2:tail2)
  
-}