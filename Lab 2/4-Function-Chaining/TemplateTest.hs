{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# OPTIONS_GHC -Wall                   #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main (main) where

import Template
import Testing

-- Tests for applyAll
-- ----------------------------------------------------------------------------

-- Unit test from the assignment
prop_apply_all_1 :: Test
prop_apply_all_1 = unitTest
                     "applyAll [(+2),(*2)] 5"
                     (applyAll [(+2),(*2)] 5)
                     (12 :: Int)

-- Unit test from the assignment
prop_apply_all_2 :: Test
prop_apply_all_2 = unitTest
                     "applyAll [(:[]). sum, filter odd] [1..8]"
                     (applyAll [(:[]). sum, filter odd] [1..8])
                     [16 :: Int]

-- If we increase m by one n times, then the result is m+n
prop_apply_all_3 :: Test
prop_apply_all_3 = randomTest
                     "forall n m. (n >= 0) ==> (applyAll (replicate n succ) m == (m + fromIntegral n))"
                     2
                     (\n (m :: Integer) -> (n >= 0) ==> (applyAll (replicate n succ) m == (m + fromIntegral n)))

-- Tests for applyTimes
-- ----------------------------------------------------------------------------

-- Unit test from the assignment
prop_apply_times_1 :: Test
prop_apply_times_1 = unitTest
                       "applyTimes 5 (+ 1) 0"
                       (applyTimes 5 (+ 1) 0)
                       (5 :: Int)

-- Unit test from the assignment
prop_apply_times_2 :: Test
prop_apply_times_2 = unitTest
                       "applyTimes 4 (++ \"i\") \"W\""
                       (applyTimes 4 (++ "i") "W")
                       "Wiiii"

-- Unit test from the assignment
prop_apply_times_3 :: Test
prop_apply_times_3 = unitTest
                       "applyTimes 0 (error \"Error!\") 3.14"
                       (applyTimes 0 (error "Error!") 3.14)
                       (3.14 :: Double)

-- If the number of times is not positive, then the result is the input
prop_apply_times_4 :: Test
prop_apply_times_4 = randomTest
                       "forall n m. (n <= 0) ==> applyTimes n succ m == m"
                       2
                       (\n (m :: Integer) -> (n <= 0) ==> applyTimes n succ m == m)

-- Tests for applyMultipleFuncs
-- ----------------------------------------------------------------------------

-- Unit test from the assignment
prop_apply_mult_1 :: Test
prop_apply_mult_1 = unitTest
                      "applyMultipleFuncs 2 [(*2), (*3), (+6)]"
                      (applyMultipleFuncs 2 [(*2), (*3), (+6)])
                      [4,6,8 :: Int]

-- Unit test (empty list of functions)
prop_apply_mult_2 :: Test
prop_apply_mult_2 = randomTest
                      "forall a. applyMultipleFuncs a [] == []"
                      1
                      (\(a :: Int) -> applyMultipleFuncs a ([] :: [Int -> Int]) == [])

-- The length is preserved
prop_apply_mult_3 :: Test
prop_apply_mult_3 = randomTest
                      "forall n. applyMultipleFuncs 0 (replicate n id) == replicate n 0"
                      1
                      (\n -> applyMultipleFuncs (0 :: Int) (replicate n id) == replicate n 0)

-- All the tests to run
allTests :: [Test]
allTests = [ prop_apply_all_1, prop_apply_all_2, prop_apply_all_3
           , prop_apply_times_1, prop_apply_times_2, prop_apply_times_3, prop_apply_times_4
           , prop_apply_mult_1, prop_apply_mult_2, prop_apply_mult_3
           ]

-- Default call
main :: IO ()
main = processSubmission allTests

-- -- Uncomment this if you want to check the tests locally, without colors
-- main :: IO ()
-- main = runAndPrintTestsLocally False allTests

-- -- Uncomment this if you want to check the tests locally, with colors enabled
-- main :: IO ()
-- main = runAndPrintTestsLocally True allTests


