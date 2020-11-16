myand :: [Bool] -> Bool
myand [] = True
myand [x] = x
myand (b:bTail) = b && myand bTail