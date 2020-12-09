import Data.Complex
import GHC.Float

type Signal = [Double] 
data Fourier =  Four [Complex Double]

mkFourier :: [Complex Double] -> Fourier
mkFourier = Four

unFourier :: Fourier -> [Complex Double]
unFourier (Four list) = list
-------------------------------------------------------------------------------
-- Almost Equal

class AlmostEq a where
  (~=) :: a -> a -> Bool

-- what is this?
infix 4 ~=

instance AlmostEq Double where
  c ~= d = (abs (c-d))<(1e-14)

instance AlmostEq (Complex Double) where
  e ~= f = (abs (magnitude (e-f)))<(1e-14)

instance AlmostEq a => AlmostEq [a] where
  [] ~= [] = True
  a  ~= [] = False
  [] ~= a  = False
  (x:xs) ~= (y:ys) = (length xs)==(length ys) && (x ~= y) && (xs ~=ys)

instance AlmostEq Fourier where
  (Four x) ~= (Four y) = x~=y



dftSignal2ComplexDouble :: Signal -> [Complex Double]
dftSignal2ComplexDouble (x:[])  = [ sum [ (helper x k n len)  |k<- kList, let x =[x]!!k] | n<-nList]
 where 
  len = length [x]
  nList = [0]
  kList = [0] 

dftSignal2ComplexDouble list =[ sum [(helper x k n len) | k <- kList, let x =[x]!!k]  | n<-nList]
 where 
  len = length list
  nList = [0..(len-1)]
  kList = [0..(len-1)] 


helper :: Double -> Int -> Int -> Int -> Complex Double
helper x k n len = ((x2) *(exp ((-2)*pi*k2*n2*(1/len2)*(0:+1))))
 where 
  x2= d2rc x
  k2= i2rc k
  n2= i2rc n
  len2= i2rc len


i2rc :: Int -> Complex Double
i2rc a = (int2Double a):+ 0 

d2rc :: Double -> Complex Double
d2rc a = a:+ 0 

-- | Convert a 'Double' to a 'Complex Double' with no real part.
d2ic :: Double -> Complex Double
d2ic a = 0:+ a