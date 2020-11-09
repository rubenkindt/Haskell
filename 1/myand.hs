myand :: [Bool] -> Bool
myand [] = False
myand [x] = x
myand (b:bTail) = b && myand bTail