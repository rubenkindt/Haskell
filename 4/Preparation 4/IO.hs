module Template where

import Control.Monad

-- * Drilling on IO
-- ----------------------------------------------------------------------------

prog1 :: IO ()
prog1 = do 
    m <- getLine
    n <- getLine
    let x = (read n :: Int)
    putStrLn n


--let x = (read n :: Int)
--iterate (print) m x
{-
helper:: Int -> Int -> ()
helper 0 _ = ()
helper x y = putStrLn y helper (x-1) y
-}

prog1b :: IO ()
prog1b = getLine >>= \x -> getLine >>= \ x n -> putStrLn n


prog2 :: IO ()
prog2 = error "Not implemented"

index :: [IO a] -> IO Int -> IO a
index = error "Not implemented"

