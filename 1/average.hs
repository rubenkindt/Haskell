addAndCount :: [Int] -> (Int,Int)

addAndCount (n:ns) = (n,1) `addTupple` add=andCount ns
  where 
    addTupple :: (Int,Int) -> (Int,Int) -> (Int,Int)
	