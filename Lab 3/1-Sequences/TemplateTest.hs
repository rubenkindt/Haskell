{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# OPTIONS_GHC -Wall                   #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main (main) where

import Template
import Testing

-- Tests for the Sequence instances
-- ----------------------------------------------------------------------------

-- Unit test from the assignment
prop_sequence_1 :: Test
prop_sequence_1 = unitTest
                    "prev \'t\'"
                    (prev 't')
                    's'

-- TODO: We need to extend the library so that we can add should-fail-tests.
-- prop_sequence_2 :: Test
-- prop_sequence_2 = unitTest
--                     "next \'z\'"
--                     (next 'z')
--                     (*** Exception: no value after 'z')

-- Unit test from the assignment
prop_sequence_3 :: Test
prop_sequence_3 = unitTest
                    "next (2 :: Integer)"
                    (next (2 :: Integer))
                    3

-- Unit test from the assignment
prop_sequence_4 :: Test
prop_sequence_4 = unitTest
                    "next True"
                    (next True)
                    False

-- Unit test from the assignment
prop_sequence_5 :: Test
prop_sequence_5 = unitTest
                    "next False"
                    (next False)
                    True


-- Tests for the Bounded instances
-- ----------------------------------------------------------------------------

-- Unit test from the assignment
prop_first_last_elem_char :: Test
prop_first_last_elem_char = unitTest
                              "firstElem :: Char"
                              (firstElem :: Char)
                              'a'

-- Unit test from the assignment
prop_first_last_elem_bool :: Test
prop_first_last_elem_bool = unitTest
                             "lastElem :: Bool"
                             (lastElem :: Bool)
                             True

-- Property (next . prev == id) holds for all apart from the first element
prop_next_prev_char :: Test
prop_next_prev_char = randomTest
                        "forall (n :: Char). ((n > firstElem) && (n <= lastElem)) ==> next (prev n) == n"
                        1
                        (\(n :: Char) -> ((n > firstElem) && (n <= lastElem)) ==> next (prev n) == n)

-- Property (next . prev == id) holds for all apart from the first element
prop_next_prev_int :: Test
prop_next_prev_int = randomTest
                        "forall (n :: Integer). next (prev n) == n"
                        1
                        (\(n :: Integer) -> next (prev n) == n)

-- Property (next . prev == id) holds for all apart from the first element
prop_next_prev_bool :: Test
prop_next_prev_bool = randomTest
                        "forall (n :: Bool). ((n > firstElem) && (n <= lastElem)) ==> next (prev n) == n"
                        1
                        (\(n :: Bool) -> ((n > firstElem) && (n <= lastElem)) ==> next (prev n) == n)

-- Property (prev . next == id) holds for all apart from the first element
prop_prev_next_int :: Test
prop_prev_next_int = randomTest
                       "prev (next n) == n"
                       1
                       (\(n :: Integer) -> prev (next n) == n)


-- Property (prev . next == id) holds for all apart from the first element
prop_prev_next_char :: Test
prop_prev_next_char = randomTest
                        "forall (n :: Char). ((n >= firstElem) && (n < lastElem)) ==> prev (next n) == n"
                        1
                        (\(n :: Char) -> ((n >= firstElem) && (n < lastElem)) ==> prev (next n) == n)

-- Property (prev . next == id) holds for all apart from the first element
prop_prev_next_bool :: Test
prop_prev_next_bool = randomTest
                        "forall (n :: Bool). ((n >= firstElem) && (n < lastElem)) ==> prev (next n) == n"
                        1
                        (\(n :: Bool) -> ((n >= firstElem) && (n < lastElem)) ==> prev (next n) == n)

-- All the tests to run
allTests :: [Test]
allTests = [ prop_sequence_1, prop_sequence_3, prop_sequence_4, prop_sequence_5
           , prop_first_last_elem_char, prop_first_last_elem_bool
           , prop_next_prev_int, prop_next_prev_char, prop_next_prev_bool
           , prop_prev_next_int, prop_prev_next_char, prop_prev_next_bool
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

