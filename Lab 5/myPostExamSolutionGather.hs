scatter :: [a] -> ([a],[a])
scatter []        =([],[])
scatter (x:[])    =([x],[])
scatter (x:xx:xs) =((x:xs1),(xx:xs2)) where
 (xs1,xs2) = scatter xs

gather :: [a] -> [a]
gather =  (\ (a, b) -> a ++ b) . scatter 