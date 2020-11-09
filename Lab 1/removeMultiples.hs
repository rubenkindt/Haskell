removeMultiples :: Int -> [Int] -> [Int]
removeMultiples veelv lijst = [nr | nr <- lijst, nr `mod` veelv /= 0]


range :: Int -> Int -> [Int]
range small big 
 |small<big = [ a | a <- [small..big]]
 |otherwise =[]