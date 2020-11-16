mapLC :: (a -> b) -> [a] -> [b]
mapLC f list=map f list


filterLC :: (a -> Bool) -> [a] -> [a]
filterLC f list=filter f list
