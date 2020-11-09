flatten :: [[Int]] -> [Int]
-- flatten [] = []
flatten multiList =[ element | a <- multiList,element <- a]
