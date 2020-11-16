{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# OPTIONS_GHC -Wall                   #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main (main) where

import Template
import Testing

-- Tests for mapLC, filterLC and concatLC (Just behave as expected)
-- ----------------------------------------------------------------------------

-- It should behave exactly like the one without list comprehensions
prop_map :: Test
prop_map = randomTest
             "forall xs. mapLC succ xs == map succ xs"
             1
             (\(xs :: [Int]) -> mapLC succ xs == map succ xs)

-- It should behave exactly like the one without list comprehensions
prop_filter :: Test
prop_filter = randomTest
                "forall xs. filterLC (>0) xs == filter (>0) xs"
                1
                (\(xs :: [Int]) -> filterLC (>0) xs == filter (>0) xs)

-- All the tests to run
allTests :: [Test]
allTests = [ prop_map, prop_filter ]

-- Default call
main :: IO ()
main = runAndPrintTestsJSON allTests

-- -- Uncomment this if you want to check the tests locally, without colors
-- main :: IO ()
-- main = runAndPrintTestsLocally False allTests

-- -- Uncomment this if you want to check the tests locally, with colors enabled
-- main :: IO ()
-- main = runAndPrintTestsLocally True allTests

