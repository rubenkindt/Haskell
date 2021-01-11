  {-# LANGUAGE FlexibleInstances #-}

module Template where

import Data.Complex

-------------------------------------------------------------------------------
-- Utility functions

-- | Convert an 'Int' to a 'Double'
i2d :: Int -> Double
i2d = fromIntegral

-- | Convert a 'Double' to a 'Complex Double' with no imaginary part.
d2rc :: Double -> Complex Double
d2rc a = a:+ 0 

-- | Convert a 'Double' to a 'Complex Double' with no real part.
d2ic :: Double -> Complex Double
d2ic a = 0:+ a

{- 1 / 1 -}

-- | A signal is a list of double precision samples
type Signal = [Double]

-- | Complete the definition of Fourier
data Fourier =  Fourier [Complex Double]


mkFourier :: [Complex Double] -> Fourier
mkFourier = Fourier

unFourier :: Fourier -> [Complex Double]
unFourier (Fourier list) = list

{- 1 / 1 -}

-------------------------------------------------------------------------------
-- Almost Equal

class AlmostEq a where
  (~=) :: a -> a -> Bool

-- what is this 4 ?
infix 4 ~=

instance AlmostEq Double where
  a ~= b = abs(a-b)<(1e-14)

instance AlmostEq (Complex Double) where
  a ~= b = abs(magnitude(a-b))<(1e-14)

instance AlmostEq a => AlmostEq [a] where
  [] ~= [] = True
  (x:xs) ~= (y:ys) = (length xs)==(length ys) && (x ~= y) && (xs ~=ys)

instance AlmostEq Fourier where
  (Fourier x) ~= (Fourier y) = x~=y

{- 1 / 1 -}

-------------------------------------------------------------------------------
-- Naive DFT

-- | Naive discrete Fourier transform
dft :: Signal -> Fourier
dft [] = (Fourier [])
dft list = Fourier (dftSignal2ComplexDouble list 0)

dftSignal2ComplexDouble :: Signal -> Int -> [Complex Double]
dftSignal2ComplexDouble [] _=[]
{-
dftSignal2ComplexDouble list index = sum (x*(exp (-2*pi*(len*index/len)*(0:+1))))
 where 
  len= length list
  x= list !! index
-}

{- 0.5 / 1 -}

-- | Naive inverse discrete Fourier transform
idft :: Fourier -> Signal
idft = error "Not implemented."

{- 0 / 1 -}

-------------------------------------------------------------------------------
-- Spectrum Analyser

spectrumAnalyser :: Signal -> IO ()
spectrumAnalyser [] = return ()
spectrumAnalyser list = show $ concat [(replicate y ("#")) ++ "\r\n" | y <- norm ]
  where 
    fur = dft list  
    max = maximum fur
    norm = [ceiling ((magnitude x)*40/max) |x<-fur]

{- 0 / 1 -}    

-------------------------------------------------------------------------------
-- Fast Fourier Transform

{-
I can see the shape of a correct solution in there, but on the exam,
make sure to comment out code that doesn't compile. We do read the comments
and give partial grades for answers that are going in the right direction.
But if your code doesn't compile, we can't even run the tests without first
finding and commenting out all the code in your file that doesn't compile,
which is really annoying.
-}


scatter :: [a] -> ([a],[a])
scatter [] = ([],[])
scatter list = ((filter (\ (x,b) xs-> if b then (x:xs) else xs) indexList), (filter (\ (x,b) xs-> if not b then (x:xs) else xs) indexList ))
  where
    indexList = scatterHelper list (concat $ replicate (length list) [True,False])

{- try to limit lines to 80 columns -}


scatterHelper :: [a] -> [Bool] -> ([a],[a])
scatterHelper list bools= zip list bools

-- | Gather the Fourier coordinates into the correct order.
-- > gather [X0, X4, X1, X5, X2, X6, X3, X7] = [X0,X1,X2,X3,X4,X5,X6,X7]
gather :: [a] -> [a]
gather =  (\ a b -> a ++ b) $ scatter

{- 0.5 /1 -}

-- | Combine even and odd Fourier coefficients with the twiddle factor.
twiddle :: Int -> Int -> Complex Double -> Complex Double -> [Complex Double]
twiddle nBig nSmall eN oN= [eN+( exp (-2*pi*nSmall*(0:+1)/nBig))*oN, eN -( exp (-2*pi*nSmall*(0:+1)/nBig))*oN]

{- 0.5 / 1 -}

-- | The butterfly step: construct Fourier coefficients from the
-- Fourier coefficients of the even an odd elements.
butterfly :: Int -> Fourier -> Fourier -> [Complex Double] -- do not nunderstand function, Applyt widdle elementwise, and concatenate the results
butterfly n (Fourier xs) (Fourier ys) = [twiddle x |x<-xs] ++ [twiddle y |y<-ys]

{- 0 / 1 -}

-- | Cooley-Tukey Fast Fourier Transform algorithm.
fft :: Signal -> Fourier
fft signa = gather butter
 where 
  (a,b)=scatter signa
  (dftA,dftB)=(dft a,dft b)
  butter = butterfly ((length dftA) + (length dft B)) dftA dftB

{- 0.5 / 1 -}

{- 5 / 10 -}

-- 
Alexander Vandenbroucke
