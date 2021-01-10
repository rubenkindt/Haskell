--not working because a 

import Data.Complex
--import GHC.Float


class AlmostEq a where
  (~=) :: a -> a -> Bool

-- what is this?
infix 4 ~=

ep :: Double
ep=1e-14

instance AlmostEq Double where
  c ~= d = (abs (c-d))<ep
  
instance AlmostEq (Complex Double) where
  a ~= b = magnitude (a - b) < e where e = 1e-14