{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Template
import Testing

import Data.List (genericReplicate)

-- * Tests for sum
-- ----------------------------------------------------------------------------

-- Gauss' sum
prop_sum_1 :: Test
prop_sum_1 = randomTest
               "forall n. (n >= 0) ==> (mySum [1..n] == ((n * (n+1)) `div` 2))"
               1
               (\n -> (n >= 0) ==> (mySum [1..n] == ((n * (n+1)) `div` 2 )))

-- Sum of m repeated n times should equal n*m
prop_sum_2 :: Test
prop_sum_2 = randomTest
               "forall n m. (n >= 0) ==> (mySum (genericReplicate n m) == (n * m))"
               2
               (\n m -> (n >= 0) ==> (mySum (genericReplicate n m) == (n * m)))

-- TODO: Add more tests for sum without giving away its implementation?
-- TODO: Add some unit tests

-- * Tests for product
-- ----------------------------------------------------------------------------

-- If the list contains a zero then the result should be zero
prop_product_1 :: Test
prop_product_1 = randomTest
                   "forall xs. elem 0 xs ==> (myProduct xs == 0)"
                   1
                   (\xs -> elem 0 xs ==> (myProduct xs == 0))

-- If the list does not contain zeros then the result is non-zero
prop_product_2 :: Test
prop_product_2 = randomTest
                   "forall xs. notElem 0 xs ==> (myProduct xs /= 0)"
                   1
                   (\xs -> notElem 0 xs ==> (myProduct xs /= 0))

-- All elements divide the product
prop_product_3 :: Test
prop_product_3 = randomTest
                   "forall xs. (myProduct xs /= 0) ==> all (\\x -> mod (myProduct xs) x == 0) xs"
                   1
                   (\xs -> (myProduct xs /= 0) ==> all (\x -> mod (myProduct xs) x == 0) xs)

-- The product of the concatenation of two lists is the product of the products
-- of each list
prop_product_4 :: Test
prop_product_4 = randomTest
                   "forall xs ys. myProduct (xs ++ ys) == ((myProduct xs) * (myProduct ys))"
                   2
                   (\xs ys ->  myProduct (xs ++ ys) == ((myProduct xs) * (myProduct ys)))

-- A unit test with positive and negative numbers
prop_product_5 :: Test
prop_product_5 = unitTest
                   "myProduct [2,-3,5,-6,-4]"
                   (myProduct [2,-3,5,-6,-4])
                   (-720)

-- || foldInts :: (Int -> Int -> Int) -> Int -> [Int] -> Int
-- || foldInts fn base ints = undefined

-- || myFoldl :: (b -> a -> b) -> b -> [a] -> b
-- || myFoldl fn base list = undefined

-- || myFoldr :: (a -> b -> b) -> b -> [a] -> b
-- || myFoldr fn base list = undefined

-- || readInBase :: Int -> [Int] -> Int
-- || readInBase base digits = undefined

-- * Tests for map
-- ----------------------------------------------------------------------------

-- Map preserves the length of the list
prop_map_1 :: Test
prop_map_1 = randomTest
               "forall xs. length xs == length (myMap id xs)"
               1
               (\(xs :: [Int]) -> length xs == length (myMap id xs))

-- Free theorem
prop_map_2 :: Test
prop_map_2 = randomTest
               "forall f xs. reverse (myMap not xs) == myMap not (reverse xs)"
               1
               (\xs -> reverse (myMap not xs) == myMap not (reverse xs))

-- If x is in the list then (f x) is in the result
prop_map_3 :: Test
prop_map_3 = randomTest
               "forall x xs. elem x xs ==> elem x (myMap id xs)"
               2
               (\x (xs :: [Int]) -> elem x xs ==> elem x (myMap id xs))

-- * Some unit tests for myMap from the assignment pdf
-- ----------------------------------------------------------------------------

prop_map_4 :: Test
prop_map_4 = unitTest
               "myMap (+1) [1,2,3,4]"
               (myMap (+1) [1,2,3,4 :: Int])
               [2,3,4,5]

prop_map_5 :: Test
prop_map_5 = unitTest
               "myMap not [True, False]"
               (myMap not [True, False])
               [False, True]

prop_map_6 :: Test
prop_map_6 = unitTest
               "myMap not []"
               (myMap not [])
               []

-- * Tests for map with folds
-- ----------------------------------------------------------------------------

-- It should behave exactly as myMap
prop_map_folds_1 :: Test
prop_map_folds_1 = randomTest
                     "forall xs. myMap (*2) xs == myMapF (*2) xs"
                     1
                     (\xs -> myMap (*2) (xs :: [Int]) == myMapF (*2) xs)

-- It should behave exactly as myMap (ensure they did not change the signature)
prop_map_folds_2 :: Test
prop_map_folds_2 = randomTest
                     "forall xs. myMap id xs == myMapF id xs"
                     1
                     (\xs -> myMap id (xs :: [Bool]) == myMapF id xs)

-- * Unit tests for myFold
-- ---------------------------------------------------------------------------

prop_fold_1 :: Test
prop_fold_1 = unitTest
              "myFoldl (+) 0 [1,2,3]"
              (myFoldl (+) 0 [1,2,3])
              (6 :: Int)

prop_fold_2 :: Test
prop_fold_2 = unitTest
              "myFoldl (-) 0 [1,2,3]"
              (myFoldl (-) 0 [1,2,3])
              ((-6) :: Int)

prop_fold_4 :: Test
prop_fold_4 = unitTest
              "myFoldl (++) \"\" [\"Hello\", \" \", \"World\"]"
              (myFoldl (++) "" ["Hello", " ", "World"])
              ("Hello World" :: String)

prop_fold_5 :: Test
prop_fold_5 = unitTest
              "myFoldr (+) 0 [1,2,3]"
              (myFoldr (+) 0 [1,2,3])
              (6 :: Int)

prop_fold_6 :: Test
prop_fold_6 = unitTest
              "myFoldr (-) 0 [1,2,3]"
              (myFoldr (-) 0 [1,2,3])
              (2 :: Int)

prop_fold_7 :: Test
prop_fold_7 = unitTest
              "myFoldr (:) [] [1,2,3]"
              (myFoldr (:) [] [1,2,3])
              ([1,2,3] :: [Int])

prop_fold_8 :: Test
prop_fold_8 = unitTest
              "myFoldr (++) \"\" [\"Hello\", \" \", \"World\"]"
              (myFoldr (++) "" ["Hello", " ", "World"])
              ("Hello World" :: String)

-- * Some more unit tests from the assignment pdf
-- ----------------------------------------------------------------------------

prop_mixed_1_9 :: Test
prop_mixed_1_9 = unitTest
                   "mySum [1,4,7,10]"
                   (mySum [1,4,7,10])
                   22

prop_mixed_1_10 :: Test
prop_mixed_1_10 = unitTest
                    "mySum []"
                    (mySum [])
                    0

prop_mixed_1_11 :: Test
prop_mixed_1_11 = unitTest
                    "myProduct [1,2,3]"
                    (myProduct [1,2,3])
                    6

prop_mixed_1_12 :: Test
prop_mixed_1_12 = unitTest
                    "myProduct []"
                    (myProduct [])
                    1

prop_mixed_1_13 :: Test
prop_mixed_1_13 = unitTest
                    "foldInts (+) 0 [1,2,3,4]"
                    (foldInts (+) 0 [1,2,3,4])
                    10

prop_mixed_1_14 :: Test
prop_mixed_1_14 = unitTest
                    "foldInts (*) 1 [1,2,3,4]"
                    (foldInts (*) 1 [1,2,3,4])
                    24

prop_mixed_1_15 :: Test
prop_mixed_1_15 = unitTest
                    "readInBase 2 [1,0]"
                    (readInBase 2 [1,0])
                    2

prop_mixed_1_16 :: Test
prop_mixed_1_16 = unitTest
                    "readInBase 6 [1,3,0]"
                    (readInBase 6 [1,3,0])
                    54

-- All the tests to run
allTests :: [Test]
allTests = [ prop_mixed_1_9, prop_mixed_1_10
           , prop_sum_1, prop_sum_2
           , prop_mixed_1_11, prop_mixed_1_12, prop_product_5
           , prop_product_1, prop_product_2, prop_product_3, prop_product_4
             -- TODO: Add tests for: foldInts
             -- TODO: Add tests for: myFoldl
             -- TODO: Add tests for: myFoldr
             -- TODO: Add tests for: readInBase
           , prop_mixed_1_13, prop_mixed_1_14
           , prop_fold_1, prop_fold_2 --, prop_fold_3
           , prop_fold_4
           , prop_fold_5, prop_fold_6, prop_fold_7, prop_fold_8
           , prop_mixed_1_15, prop_mixed_1_16
           , prop_map_1, prop_map_2, prop_map_3
           , prop_map_4, prop_map_5, prop_map_6
           , prop_map_folds_1, prop_map_folds_2
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

