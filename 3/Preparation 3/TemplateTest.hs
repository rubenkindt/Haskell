{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# OPTIONS_GHC -Wall                   #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main (main) where

import Template
import Testing

-- Tests for Eq
-- ---------------------------------------------------------------------------

prop_eq_1 :: Test
prop_eq_1 = unitTest
            "MyTrue == MyTrue"
            (MyTrue == MyTrue)
            (True :: Bool)

prop_eq_2 :: Test
prop_eq_2 = unitTest
            "MyTrue == MyFalse"
            (MyTrue == MyFalse)
            (False :: Bool)

prop_eq_3 :: Test
prop_eq_3 = unitTest
            "(And (Const MyTrue) (Const MyFalse)) == (And (Const MyTrue) (Const MyFalse))"
            ((And (Const MyTrue) (Const MyFalse)) == (And (Const MyTrue) (Const MyFalse)))
            (True :: Bool)

prop_eq_4 :: Test
prop_eq_4 = unitTest
            "(And (Const MyTrue) (Const MyFalse)) == (Const MyFalse)"
            ((And (Const MyTrue) (Const MyFalse)) == (Const MyFalse))
            (False :: Bool)

-- Tests for show
-- ---------------------------------------------------------------------------

prop_show_1 :: Test
prop_show_1 = unitTest
              "show MyTrue"
              (show MyTrue)
              ("True" :: String)

prop_show_2 :: Test
prop_show_2 = unitTest
              "show MyFalse"
              (show MyFalse)
              ("False" :: String)

prop_show_3 :: Test
prop_show_3 = unitTest
              "show (And (Const MyTrue) (Const MyFalse))"
              (show (And (Const MyTrue) (Const MyFalse)))
              ("True && False" :: String)

prop_show_4 :: Test
prop_show_4 = unitTest
              "show (Or (And (Const MyTrue) (Const MyFalse)) (Const MyTrue))"
              (show (Or (And (Const MyTrue) (Const MyFalse)) (Const MyTrue)))
              ("True && False || True" :: String)

-- Tests for eval
-- ---------------------------------------------------------------------------

prop_eval_1 :: Test
prop_eval_1 = unitTest
              "eval MyFalse"
              (eval MyFalse)
              (False :: Bool)

prop_eval_2 :: Test
prop_eval_2 = unitTest
              "eval (Const MyTrue)"
              (eval (Const MyTrue))
              (True :: Bool)

prop_eval_3 :: Test
prop_eval_3 = unitTest
              "eval (And (Or (Const MyFalse) (Const MyTrue)) (Const MyTrue))"
              (eval (And (Or (Const MyFalse) (Const MyTrue)) (Const MyTrue)))
              (True :: Bool)

-- All the tests to run
allTests :: [Test]
allTests = [ prop_eq_1, prop_eq_2, prop_eq_3, prop_eq_4
           , prop_show_1, prop_show_2, prop_show_3, prop_show_4
           , prop_eval_1, prop_eval_2, prop_eval_3
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


