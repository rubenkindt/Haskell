--spectrumAnalyser [6 :+ 0, (-1) :+ (-1), 0:+ 0, (-1) :+ 1]
import Data.Complex

spectrumAnalyser :: [Complex Double] -> IO ()
spectrumAnalyser [] = return ()
spectrumAnalyser list = putStr $ concat [(concat (replicate y ("#"))) ++ "\r\n" | y <- norm ]
  where 
    fur = [magnitude f | f<-list]   -- removed dft, since it is missing from this code snippet
    max = maximum fur
    norm = [ceiling ((x)*40/max) |x<-fur]