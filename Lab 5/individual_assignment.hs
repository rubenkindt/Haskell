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

-- | A signal is a list of double precision samples
type Signal = [Double]

-- | Complete the definition of Fourier
data Fourier =  Fourier [Complex Double]


mkFourier :: [Complex Double] -> Fourier
mkFourier = Fourier

unFourier :: Fourier -> [Complex Double]
unFourier (Fourier list) = list

-------------------------------------------------------------------------------
-- Almost Equal

class AlmostEq a where
  (~=) :: a -> a -> Bool

-- what is this?
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

-- | Naive inverse discrete Fourier transform
idft :: Fourier -> Signal
idft = error "Not implemented."

-------------------------------------------------------------------------------
-- Spectrum Analyser

spectrumAnalyser :: Signal -> IO ()
spectrumAnalyser [] = return ()
spectrumAnalyser list = show $ concat [(replicate y ("#")) ++ "\r\n" | y <- norm ]
  where 
    fur = dft list  
    max = maximum fur
    norm = [ceiling ((magnitude x)*40/max) |x<-fur]

-------------------------------------------------------------------------------
-- Fast Fourier Transform

scatter :: [a] -> ([a],[a])
scatter [] = ([],[])
scatter list = ((filter (\ (x,b) xs-> if b then (x:xs) else xs) indexList), (filter (\ (x,b) xs-> if not b then (x:xs) else xs) indexList )) 
 where
  indexList = scatterHelper list (concat $ replicate (length list) [True,False])

scatterHelper :: [a] -> [Bool] -> ([a],[a])
scatterHelper list bools= zip list bools

-- | Gather the Fourier coordinates into the correct order.
-- > gather [X0, X4, X1, X5, X2, X6, X3, X7] = [X0,X1,X2,X3,X4,X5,X6,X7]
gather :: [a] -> [a]
gather =  (\ a b -> a ++ b) $ scatter


-- | Combine even and odd Fourier coefficients with the twiddle factor.
twiddle :: Int -> Int -> Complex Double -> Complex Double -> [Complex Double]
twiddle nBig nSmall eN oN= [eN+( exp (-2*pi*nSmall*(0:+1)/nBig))*oN, eN -( exp (-2*pi*nSmall*(0:+1)/nBig))*oN]



-- | The butterfly step: construct Fourier coefficients from the
-- Fourier coefficients of the even an odd elements.
butterfly :: Int -> Fourier -> Fourier -> [Complex Double] -- do not nunderstand function, Applyt widdle elementwise, and concatenate the results
butterfly n (Fourier xs) (Fourier ys) = [twiddle x |x<-xs] ++ [twiddle y |y<-ys]

-- | Cooley-Tukey Fast Fourier Transform algorithm.
fft :: Signal -> Fourier
fft signa = gather butter
 where 
  (a,b)=scatter signa
  (dftA,dftB)=(dft a,dft b)
  butter = butterfly ((length dftA) + (length dft B)) dftA dftB

