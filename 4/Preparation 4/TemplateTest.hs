{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# OPTIONS_GHC -Wall                    #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

module Main (main) where

import Template
import Testing
import Control.Monad
import Control.Applicative

-- Unit Tests for prog1
-- ----------------------------------------------------------------------------

prop_prog1_1 :: Test
prop_prog1_1 = unitTestIO "Print 2, 3 times" prog1 ["3","2"] ["2","2","2"]




prop_prog1_2 :: Test
prop_prog1_2 = unitTestIO "Print 55, 4 times" prog1 ["4","55"] ["55","55","55","55"]


-- Unit Tests for prog1b
-- ----------------------------------------------------------------------------

prop_prog1b_1 :: Test
prop_prog1b_1 = unitTestIO "Print 2, 3 times" prog1b ["3","2"] ["2","2","2"]




prop_prog1b_2 :: Test
prop_prog1b_2 = unitTestIO "Print 55, 4 times" prog1b ["4","55"] ["55","55","55","55"]




-- Unit Tests for prog2
-- ----------------------------------------------------------------------------

prop_prog2_1 :: Test
prop_prog2_1 = unitTestIO "Revert test and Computer" prog2 ["test","Computer",""] ["\"tset\"","\"retupmoC\""]




prop_prog2_2 :: Test
prop_prog2_2 = unitTestIO "test" prog2 ["Haskell","Is Fun!",""] ["\"lleksaH\"","\"!nuF sI\""]


-- Unit Tests for index
-- ----------------------------------------------------------------------------

prop_index_1 :: Test
prop_index_1 = unitTestIO "test" (index [print "Hello World",print "Hello Galaxy"] readLn) ["1"] ["\"Hello Galaxy\""]

prop_index_2 :: Test
prop_index_2 = unitTestIO "test" (index [print "Hello World",print "Hello Galaxy"] readLn) ["0"] ["\"Hello World\""]


-- All the tests to run
allTests :: [Test]
allTests = [  prop_prog1_1 , prop_prog1_2
            , prop_prog1b_1, prop_prog1b_2
            , prop_prog2_1 , prop_prog2_2
            , prop_index_1 , prop_index_2 ]

-- Default call
main :: IO ()
main = processSubmission allTests

-- -- Uncomment this if you want to check the tests locally, without colors
-- main :: IO ()
-- main = runAndPrintTestsLocally False allTests

-- -- Uncomment this if you want to check the tests locally, with colors enabled
-- main :: IO ()
-- main = runAndPrintTestsLocally True allTests
