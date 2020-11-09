range :: Int -> Int -> [Int]
range small big 
 |small<big = [ a | a <- [small..big]]
 |otherwise =[]