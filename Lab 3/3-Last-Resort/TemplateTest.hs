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

import Data.List (sort,intersect,union)

-- Utility Function
-- ----------------------------------------------------------------------------

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:xys) = x <= y && sorted (y:xys)

disjoint :: Eq a => ([a] , [a]) -> Bool
disjoint (xs,ys) = xs `intersect` ys == []


-- Unit Tests for selectionSort
-- ----------------------------------------------------------------------------

prop_selectionSort_1 :: Test
prop_selectionSort_1 =
  randomTest
    "forall (xs :: [Int]). length (selectionSort xs) == length xs"
    1
    (\(xs :: [Int]) -> length (selectionSort xs) == length xs)

prop_selectionSort_2 :: Test
prop_selectionSort_2 =
  randomTest
    "forall (xs :: [Int]). sorted (selectionSort xs)"
    1
    (\(xs :: [Int]) -> sorted (selectionSort xs))

prop_selectionSort_3 :: Test
prop_selectionSort_3 =
  randomTest
    "forall (xs :: [Int]). selectionSort xs == sort xs"
    1
    (\(xs :: [Int]) -> selectionSort xs == sort xs)
  
-- Unit Tests for partitioning
-- ----------------------------------------------------------------------------

prop_partitionFold_1 :: Test
prop_partitionFold_1 =
  randomTest
    "partitionFold returns disjoint lists"
    1
    (\(xs :: [Int]) -> disjoint $ partitionFold (\x -> x `mod` 3 == 0) xs)

prop_partitionFold_2 :: Test
prop_partitionFold_2 =
  randomTest
    "partitionFold sums to the input list"
    1
    (\(xs :: [Int]) -> (sort xs) == (sort $ uncurry (++) (partitionFold (\x -> x `mod` 3 == 0) xs)))


prop_partitionFilter_1 :: Test
prop_partitionFilter_1 =
  randomTest
    "partitionFilter returns disjoint lists"
    1
    (\(xs :: [Int]) -> disjoint $ partitionFilter (\x -> x `mod` 3 == 0) xs)

prop_partitionFilter_2 :: Test
prop_partitionFilter_2 =
  randomTest
    "partitionFilter sums to the input list"
    1
    (\(xs :: [Int]) -> (sort xs) == (sort $ uncurry (++) (partitionFilter (\x -> x `mod` 3 == 0) xs)))

prop_partitionLC_1 :: Test
prop_partitionLC_1 =
  randomTest
    "partitionLC returns disjoint lists"
    1
    (\(xs :: [Int]) -> disjoint $ partitionLC (\x -> x `mod` 3 == 0) xs)

prop_partitionLC_2 :: Test
prop_partitionLC_2 =
  randomTest
    "partitionLC sums to the input list"
    1
    (\(xs :: [Int]) -> (sort xs) == (sort $ uncurry (++) (partitionLC (\x -> x `mod` 3 == 0) xs)))

-- Unit Tests for partitioning
-- ----------------------------------------------------------------------------

prop_quicksort_1 :: Test
prop_quicksort_1 =
  randomTest
    "forall (xs :: [Int]). length (quicksort xs) == length xs"
    1
    (\(xs :: [Int]) -> length (quicksort xs) == length xs)

prop_quicksort_2 :: Test
prop_quicksort_2 =
  randomTest
    "forall (xs :: [Int]). sorted (quicksort xs)"
    1
    (\(xs :: [Int]) -> sorted (quicksort xs))

prop_quicksort_3 :: Test
prop_quicksort_3 =
  randomTest
    "forall (xs :: [Int]). quicksort xs == sort xs"
    1
    (\(xs :: [Int]) -> quicksort xs == sort xs)

-- All the tests to run
allTests :: [Test]
allTests = [
  prop_selectionSort_1, prop_selectionSort_2, prop_selectionSort_3,
  prop_partitionFold_1, prop_partitionFold_2,
  prop_partitionFilter_1, prop_partitionFilter_2,
  prop_partitionLC_1, prop_partitionLC_2,
  prop_quicksort_1,prop_quicksort_2, prop_quicksort_3]

-- Default call
main :: IO ()
main = processSubmission allTests

-- -- Uncomment this if you want to check the tests locally, without colors
-- main :: IO ()
-- main = runAndPrintTestsLocally False allTests

-- -- Uncomment this if you want to check the tests locally, with colors enabled
-- main :: IO ()
-- main = runAndPrintTestsLocally True allTests

