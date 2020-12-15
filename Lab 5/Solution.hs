{-# LANGUAGE FlexibleInstances #-}

module Solution where

import Data.Complex

-------------------------------------------------------------------------------
-- Utility functions

i2d :: Int -> Double
i2d = fromIntegral

d2rc :: Double -> Complex Double
d2rc d = d :+ 0

d2ic :: Double -> Complex Double
d2ic d = 0 :+ d

type Signal = [Double]

data Fourier = MkFourier [Complex Double]


mkFourier :: [Complex Double] -> Fourier
mkFourier = MkFourier

unFourier :: Fourier -> [Complex Double]
unFourier (MkFourier fr) = fr

-------------------------------------------------------------------------------
-- Almost Equal

class AlmostEq a where
  (~=) :: a -> a -> Bool

infix 4 ~=

instance AlmostEq Double where
  a ~= b = abs (a - b) < e where e = 1e-14

instance AlmostEq (Complex Double) where
  a ~= b = magnitude (a - b) < e where e = 1e-14

instance AlmostEq a => AlmostEq [a] where
  xs ~= ys = length xs == length ys && and (zipWith (~=) xs ys)

instance AlmostEq Fourier where
  xs ~= ys = unFourier xs ~= unFourier ys

-------------------------------------------------------------------------------
-- Naive DFT

dft :: Signal -> Fourier
dft signal =
  let coef k = sum [term k j xj | (j,xj) <- zip [0..] signal]
      term k j xj = d2rc xj * exp (d2ic (-2*pi * i2d (k*j) / i2d n))
      n = length signal
  in MkFourier [coef k | k <- [0..(n - 1)]]


idft :: Fourier -> Signal
idft (MkFourier fs) =
  let n = length fs
      coef k = sum [term k j xj | (j,xj) <- zip [0..] fs]
      term k j xj = xj * exp (d2ic (2*pi * i2d (k*j) / i2d n))
  in [realPart (coef k) / i2d n | k <- [0..(n-1)]]

-------------------------------------------------------------------------------
-- Spectrum Analyser

spectrumAnalyser :: Signal -> IO ()
spectrumAnalyser signal =
  let fs = map magnitude $ unFourier $ fft signal
      m = maximum fs
      scale = 40 / m
      bar n = replicate n '#'
      rescale c = scale * c
  in mapM_ (putStrLn . bar . ceiling . rescale) fs

-- If you want to have some fun with fancy looking bars, uncomment this.

-- spectrumAnalyser :: Signal -> IO ()
-- spectrumAnalyser signal =
--   let fs = map magnitude $ unFourier $ fft signal
--       m = maximum fs
--       scale = 40 / m
--       darkblock = toEnum 0x2588
--       midblock = toEnum 0x2592
--       lightblock = toEnum 0x2591
--       bar n = concat [
--         replicate (n-1) midblock,
--         [darkblock | n > 0],
--         replicate (40 - n) lightblock ]
--       rescale c = scale * c
--   in mapM_ (putStrLn . bar . ceiling . rescale) fs

-------------------------------------------------------------------------------
-- Fast Fourier Transform

scatter :: [a] -> ([a],[a])
scatter = foldr (\x (ys,xs) -> (x:xs,ys)) ([],[])
          -- assume length of list is even

gather :: [a] -> [a]
gather = uncurry (++) . scatter

twiddle :: Int -> Int -> Complex Double -> Complex Double -> [Complex Double]
twiddle n k e o =
  let t = exp (d2ic (-2*pi*i2d k / i2d n))
  in [e + t*o, e - t*o]

butterfly :: Int -> Fourier -> Fourier -> [Complex Double]
butterfly n xs ys =
  concat $ zipWith3 (twiddle n) [0..] (unFourier xs) (unFourier ys)

fft :: Signal -> Fourier
fft [] = mkFourier []
fft [x] = mkFourier [d2rc x]
fft signal =
  let n = length signal
      bimap f (x,y) = (f x, f y)
  in if even n then -- OPTIONAL part
       mkFourier $ gather $ uncurry (butterfly n) $ bimap fft $ scatter signal
     else
       dft signal
