module Template where

import Control.Monad

-- * Drilling on IO
-- ----------------------------------------------------------------------------

prog1 :: IO ()
prog1 = do
  x <- readLn :: IO Int
  y <- readLn :: IO Int
  replicateM_ x $ putStrLn $ show y

{-
prog1 :: IO ()
prog1 = do
  x <- readLn :: IO Int
  y <- getLine
  replicateM_ x $ putStrLn $ y
-}

prog1b :: IO ()
prog1b = (readLn :: IO Int) >>= \ x -> ((readLn :: IO Int) >>= (\ y -> (replicateM_ x $ putStrLn $ show y)))

prog2 :: IO ()
prog2 = do
 x <- getLine
 if x == "" then return () else do 
    _ <- putStrLn $ show $ reverse x
    prog2
{-
prog2 :: IO ()
prog2 = do
 x <- getLine
 if x == "" then return () else putStrLn $ show $ reverse x
 if x == "" then return () else prog2
-} 


index :: [IO a] -> IO Int -> IO a
index list indexx = indexx >>= (\indexx -> list !! indexx)
-- index [IO a] -> IO Int -> IO a  
-- [IO a] IO Int = IO Int >>= (Int -> IO a) -> IO a

{-
return :: a -> m a
(>>=) :: m a -> (a -> m b) -> m b
(>>=) :: IO a -> ( a -> IO b) -> IO b
-}

