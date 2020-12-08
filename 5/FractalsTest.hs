module FractalsTest where

import Fractals
import Control.Monad
import Data.Maybe (isNothing, isJust)
import Prelude hiding (catch, foldr1, foldl1) -- Required for older versions of GHC
import Data.Monoid
import Control.Exception (catch, SomeException(..))

-- Some Tests
main :: IO ()
main = startTests
       -- ~~~~~~~~~~~~~~~~~~~~~~~ PART 1 ~~~~~~~~~~~~~~~~~~~~~~~
       <> test "(done >>> done == done)"
           (done >>> done == done)
       <> test "(turn 50 >>> done == turn 50)"
           (turn 50 >>> done == turn 50)
       <> test "(step 10 >>> done == step 10)"
           (step 10 >>> done == step 10)
       <> test "(step 10 >>> turn 50 /= turn 50 >>> step 10)"
           (step 10 >>> turn 50 /= turn 50 >>> step 10)
       <> test "(step 10 >>> turn 50 /= step 10)"
           (step 10 >>> turn 50 /= step 10)
       <> test "(step 10 >>> (turn 50 >>> step 20) == (step 10 >>> turn 50) >>> step 20)"
           (step 10 >>> (turn 50 >>> step 20) == (step 10 >>> turn 50) >>> step 20)
       -- ~~~~~~~~~~~~~~~~~~~~~~~ PART 2 ~~~~~~~~~~~~~~~~~~~~~~~
       <> test "(turtleToLines done == [])"
           (turtleToLines done == [])
       <> test "(turtleToLines (turn 90) == [])"
           (turtleToLines (turn 90) == [])
       <> test "(turtleToLines (turn 90 >>> turn 45) == [])"
           (turtleToLines (turn 90 >>> turn 45) == [])
       <> test "(turtleToLines (step 10) == [((500,500),(500,510))])"
           (turtleToLines (step 10) == [((500,500),(500,510))])
       <> test "(turtleToLines (turn 90 >>> step 10) == [((500,500),(500,510))])"
           (turtleToLines (turn 90 >>> step 10) == [((500,500),(510,500))])
       <> test "(turtleToLines (turn (-90) >>> step 10) == [((500,500),(490,500))])"
           (turtleToLines (turn (-90) >>> step 10) == [((500,500),(490,500))])
       <> test "(turtleToLines (turn (-180) >>> step 10) == [((500,500),(500,490))])"
           (turtleToLines (turn (-180) >>> step 10) == [((500,500),(500,490))])
       -- ~~~~~~~~~~~~~~~~~~~~~~~ PART 3 ~~~~~~~~~~~~~~~~~~~~~~~
       <> test "(fdone >-> fdone == fdone)"
           (fdone >-> fdone == fdone)
       <> test "(fturn 50 >-> fdone == fturn 50)"
           (fturn 50 >-> fdone == fturn 50)
       <> test "(fstep >-> fdone == fstep)"
           (fstep >-> fdone == fstep)
       <> test "(fstep >-> fturn 50 /= fturn 50 >-> fstep)"
           (fstep >-> fturn 50 /= fturn 50 >-> fstep)
       <> test "(fstep >-> fturn 50 /= fstep)"
           (fstep >-> fturn 50 /= fstep)
       <> test "(fstep >-> (fturn 50 >-> fstep) == (fstep >-> fturn 50) >-> fstep)"
           (fstep >-> (fturn 50 >-> fstep) == (fstep >-> fturn 50) >-> fstep)
       <> test "(concretize 10 fdone == done)"
           (concretize 10 fdone == done)
       <> test "(concretize 10 (fturn 20) == turn 20)"
           (concretize 10 (fturn 20) == turn 20)
       <> test "(concretize 10 fstep  == step 10)"
           (concretize 10 fstep == step 10)
       <> test "(concretize 20 (fstep >-> fturn 50 >-> fstep) == step 20 >>> turn 50 >>> step 20)"
           (concretize 20 (fstep >-> fturn 50 >-> fstep) == step 20 >>> turn 50 >>> step 20)
       <> test "(refine fstep fstep == fstep)"
           (refine fstep fstep == fstep)
       <> test "(refine (fstep >-> fstep) fstep == fstep >-> fstep)"
           (refine (fstep >-> fstep) fstep == fstep >-> fstep)
       <> test "(refine (fstep >-> fstep) (fstep >-> fturn 50) == fstep >-> fstep >-> fturn 50)"
           (refine (fstep >-> fstep) (fstep >-> fturn 50) == fstep >-> fstep >-> fturn 50)
       <> test "(refine (fstep >-> fstep) (fstep >-> fturn 50 >-> fstep) == fstep >-> fstep >-> fturn 50 >-> fstep >-> fstep)"
           (refine (fstep >-> fstep) (fstep >-> fturn 50 >-> fstep) == fstep >-> fstep >-> fturn 50 >-> fstep >-> fstep)
       <> test "(times 0 (+7) 42 == 42)"
           (times 0 (+7) 42 == 42)
       <> test "(times 1 (+7) 42 == 49)"
           (times 1 (+7) 42 == 49)
       <> test "(times 5 (+1) 2 == 7)"
           (times 5 (+1) 2 == 7)
       <> test "(times 5 (*2) 2 == 64)"
           (times 5 (*2) 2 == 64)
       -- ~~~~~~~~~~~~~~~~~~~~~~~ END ~~~~~~~~~~~~~~~~~~~~~~~
       >>= endTests

-- Mini testing framework
test :: String -> Bool -> IO Results
test msg b
  = do notImplemented <- isUndefined b
       case notImplemented of
         True      -> printResult yellow "function not implemented" >> return (Sum 1, Sum 0, Sum 0)
         False | b -> printResult green "passed" >> return (Sum 0, Sum 0, Sum 1)
         _         -> printResult red "failed" >> return (Sum 0, Sum 1, Sum 0)
  where printResult colorCode result
          = do putStrLn "--------------------------------------------------------------------------------"
               putStrLn $ "-> Test " ++ msg ++ " " ++ colorise colorCode result

type Results = (Sum Int, Sum Int, Sum Int) -- (Not implemented, failed, passed)

-- OBSOLOTE: already in standard library
-- 
-- instance Monoid a => Monoid (IO a) where
--  mempty = return mempty
--  mappend = liftM2 mappend

startTests :: IO Results
startTests = putStrLn "Testing your solutions" >> return (Sum 0, Sum 0, Sum 0)

endTests :: Results -> IO ()
endTests (notImpl, failed, passed)
  = do putStrLn "--------------------------------------------------------------------------------"
       case (getSum notImpl, getSum failed, getSum passed) of
         (0, 0, _) -> putStrLn $ colorise green "All tests passed"
         (n, f, p) -> putStrLn $ unwords $
                        filter (not . null) [nNotImpl n, nFailed f, nPassed p]
  where nPassed 0 = ""
        nPassed p = colorise green $ "=> " ++  show p ++ " tests passed"
        nFailed 0 = ""
        nFailed f = colorise red $ "=> " ++  show f ++ " tests failed"
        nNotImpl 0 = ""
        nNotImpl n = colorise yellow $ "=> " ++ show n ++ "x function not implemented"

isUndefined :: a -> IO Bool
isUndefined a = (a `seq` return False) `catch` \(SomeException _) -> return True

red, green, yellow :: Int
(red, green, yellow) = (31, 32, 33)

colorise :: Int -> String -> String
colorise colorCode s = "\ESC[0;" ++ show colorCode ++ "m" ++ s ++ "\ESC[0m"
