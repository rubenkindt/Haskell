myRepeat :: Int -> Int -> [Int]
myRepeat keer nr
 |keer >= 0 = [ nr | a <- [1..keer]]
 |otherwise =[]

