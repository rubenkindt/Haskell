{-# LANGUAGE FlexibleInstances #-}
{- this ^ prevents the error below
myPostExamSolutionDft.hs:28:10: error:
    * Illegal instance declaration for `AlmostEq (Complex Double)'
        (All instance types must be of the form (T a1 ... an)
         where a1 ... an are *distinct type variables*,
         and each type variable appears at most once in the instance head.
         Use FlexibleInstances if you want to disable this.)
    * In the instance declaration for `AlmostEq (Complex Double)'
-}

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

ep :: Double
ep=1e-14

instance AlmostEq Double where
  c ~= d = (abs (c-d))<ep

instance AlmostEq (Complex Double) where
  e ~= f = (magnitude (e-f))<1e-14

instance AlmostEq a => AlmostEq [a] where
  [] ~= [] = True
  a  ~= [] = False
  [] ~= a  = False
  (x:xs) ~= (y:ys) = (length xs)==(length ys) && (x ~= y) && (xs ~=ys)

instance AlmostEq Fourier where
  (Four x) ~= (Four y) = x~=y


-- | Naive discrete Fourier transform
dft :: Signal -> Fourier
dft [] = (Four [])
dft list = Four (dftSignal2ComplexDouble list)


dftSignal2ComplexDouble :: Signal -> [Complex Double]
dftSignal2ComplexDouble [] = []
dftSignal2ComplexDouble (x:[])  = [ sum [ (helperDft x k n len)  |k<- kList, let x =[x]!!k] | n<-nList]
 where 
  len = length [x]
  nList = [0]
  kList = [0] 

dftSignal2ComplexDouble xlist =[ sum [(helperDft x k n len) | k <- kList, let x =xlist!!k]  | n<-nList]
 where 
  len = length xlist
  nList = [0..(len-1)]
  kList = [0..(len-1)] 


helperDft :: Double -> Int -> Int -> Int -> Complex Double
helperDft x k n len = ((x2) *(exp ((-2)*pi*k2*n2*(1/len2)*(0:+1))))
 where 
  x2= d2rc x
  k2= i2rc k
  n2= i2rc n
  len2= i2rc len


-- | Naive inverse discrete Fourier transform
idft :: Fourier -> Signal
idft (Four fList)= idftF2Sig fList



idftF2Sig :: [Complex Double] -> [Double]
idftF2Sig [] = []
idftF2Sig (x:[])  = [ sum [ (1*len) * (helperIdft x k n len)  | n<-nList, let x =[x]!!k] | k<-kList ]
 where 
  len = length [x]
  nList = [0]
  kList = [0] 
idftF2Sig xlist =[ sum [(1*len) * (helperIdft x k n len) | n <- nList, let x =xlist!!k]  | k<-kList ]
 where 
  len = length xlist
  nList = [0..(len-1)]
  kList = [0..(len-1)] 


helperIdft :: Complex Double -> Int -> Int -> Int -> Double
helperIdft x k n len = ((x2) *(exp ((2)*pi*k2*n2*(1/len2)*(0:+1))))
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

