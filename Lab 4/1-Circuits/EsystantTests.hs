{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# OPTIONS_GHC -Wall                   #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main (main) where

import MyHaskell
import Testing
import Data.List (sort)

-- Tests Part 1
-- ----------------------------------------------------------------------------

prop_1b2a_1 :: Test
prop_1b2a_1 = unitTest
              "show (cinput \"x\") == \"x\""
              (show (cinput "x") == "x")
              True

prop_1b2a_2 :: Test
prop_1b2a_2 = unitTest
              "show (cnot (cinput \"x\")) == \"NOT(x)\""
              (show (cnot (cinput "x")) == "NOT(x)")
              True

prop_1b2a_3 :: Test
prop_1b2a_3 = unitTest
              "show (cand (cinput \"x\") (cinput \"y\")) == \"AND(x,y)\""
              (show (cand (cinput "x") (cinput "y")) == "AND(x,y)")
              True

prop_1b2a_4 :: Test
prop_1b2a_4 = unitTest
              "show (cor (cinput \"x\") (cinput \"y\")) == \"OR(x,y)\""
              (show (cor (cinput "x") (cinput "y")) == "OR(x,y)")
              True

prop_1b2a_5 :: Test
prop_1b2a_5 = unitTest
              "show (cxor (cinput \"x\") (cinput \"y\")) == \"XOR(x,y)\""
              (show (cxor (cinput "x") (cinput "y")) == "XOR(x,y)")
              True

prop_1c2a_1 :: Test
prop_1c2a_1 = unitTest
              "show example == \"OR(AND(x,y),XOR(NOT(z),x))\""
              (show example == "OR(AND(x,y),XOR(NOT(z),x))")
              True

prop_1d2a_1 :: Test
prop_1d2a_1 = unitTest
              "show (candMany [cinput \"x\"]) == \"x\""
              (show (candMany [cinput "x"]) == "x")
              True

-- Tests Part 2
-- ----------------------------------------------------------------------------

-- 2b
prop_2b2a_1 :: Test
prop_2b2a_1 = unitTest
              "show (simplify (cinput \"x\")) == show (cinput \"x\")"
              (show (simplify (cinput "x")) == show (cinput "x"))
              True

prop_2b2a_2 :: Test
prop_2b2a_2 = unitTest
              "show (simplify (cand (cinput \"x\") (cinput \"y\"))) == show (cand (cinput \"x\") (cinput \"y\"))"
              (show (simplify (cand (cinput "x") (cinput "y"))) == show (cand (cinput "x") (cinput "y")))
              True

prop_2b2a_3 :: Test
prop_2b2a_3 = unitTest
              "show (simplify (cor (cinput \"x\") (cinput \"y\"))) == show (cnot (cand (cnot (cinput \"x\")) (cnot (cinput \"y\"))))"
              (show (simplify (cor (cinput "x") (cinput "y"))) == show (cnot (cand (cnot (cinput "x")) (cnot (cinput "y")))))
              True

prop_2b2a_4 :: Test
prop_2b2a_4 = unitTest
              "show (simplify (xcor (cinput \"x\") (cinput \"y\"))) == show (cnot (cand (cnot (cand (cinput \"x\") (cnot (cinput \"y\")))) (cnot (cand (cnot (cinput \"x\")) (cinput \"y\")))))"
              (show (simplify (cxor (cinput "x") (cinput "y"))) == show (cnot (cand (cnot (cand (cinput "x") (cnot (cinput "y")))) (cnot (cand (cnot (cinput "x")) (cinput "y"))))))
              True

-- 2c
prop_2c_1 :: Test
prop_2c_1 = unitTest
            "size (cinput \"x\") == 0"
            (size (cinput "x") == 0)
            True

prop_2c_2 :: Test
prop_2c_2 = unitTest
            "size (cnot (cinput \"x\")) == 1"
            (size (cnot (cinput "x")) == 1)
            True

prop_2c_3 :: Test
prop_2c_3 = unitTest
            "size (cand (cinput \"x\") (cinput \"y\")) == 1"
            (size (cand (cinput "x") (cinput "y")) == 1)
            True

prop_2c_4 :: Test
prop_2c_4 = unitTest
            "size (cor (cinput \"x\") (cinput \"y\")) == 1"
            (size (cor (cinput "x") (cinput "y")) == 1)
            True

prop_2c_5 :: Test
prop_2c_5 = unitTest
            "size (cxor (cinput \"x\") (cinput \"y\")) == 1"
            (size (cxor (cinput "x") (cinput "y")) == 1)
            True

prop_2c_6 :: Test
prop_2c_6 = unitTest
            "size example == 4"
            (size example == 4)
            True

prop_2c_7 :: Test
prop_2c_7 = unitTest
            "size (simplify example) == 15"
            (size (simplify example) == 15)
            True

-- 2d
prop_2d_1 :: Test
prop_2d_1 = unitTest
            "gateDelay (cinput \"x\") == 0"
            (gateDelay (cinput "x") == 0)
            True

prop_2d_2 :: Test
prop_2d_2 = unitTest
            "gateDelay (cnot (cinput \"x\")) == 1"
            (gateDelay (cnot (cinput "x")) == 1)
            True

prop_2d_3 :: Test
prop_2d_3 = unitTest
            "gateDelay (cand (cinput \"x\") (cinput \"y\")) == 1"
            (gateDelay (cand (cinput "x") (cinput "y")) == 1)
            True

prop_2d_4 :: Test
prop_2d_4 = unitTest
            "gateDelay (cor (cinput \"x\") (cinput \"y\")) == 1"
            (gateDelay (cor (cinput "x") (cinput "y")) == 1)
            True

prop_2d_5 :: Test
prop_2d_5 = unitTest
            "gateDelay (cxor (cinput \"x\") (cinput \"y\")) == 1"
            (gateDelay (cxor (cinput "x") (cinput "y")) == 1)
            True

prop_2d_6 :: Test
prop_2d_6 = unitTest
            "gateDelay example == 3"
            (gateDelay example == 3)
            True

prop_2d_7 :: Test
prop_2d_7 = unitTest
            "gateDelay (simplify example) == 9"
            (gateDelay (simplify example) == 9)
            True

-- 2e
prop_2e_1 :: Test
prop_2e_1 = unitTest
            "inputs (cinput \"x\") == [\"x\"]"
            (inputs (cinput "x") == ["x"])
            True

prop_2e_2 :: Test
prop_2e_2 = unitTest
            "inputs (cnot (cinput \"x\")) == [\"x\"]"
            (inputs (cnot (cinput "x")) == ["x"])
            True

prop_2e_3 :: Test
prop_2e_3 = unitTest
            "sort (inputs (cand (cinput \"x\") (cinput \"y\"))) == [\"x\",\"y\"]"
            (sort (inputs (cand (cinput "x") (cinput "y"))) == ["x","y"])
            True

prop_2e_4 :: Test
prop_2e_4 = unitTest
            "length (inputs (cand (cinput \"x\") (cinput \"x\"))) == 1"
            (length (inputs (cand (cinput "x") (cinput "x"))) == 1)
            True

prop_2e_5 :: Test
prop_2e_5 = unitTest
            "sort (inputs example) == [\"x\",\"y\",\"z\"]"
            (sort (inputs example) == ["x","y","z"])
            True

prop_2e_6 :: Test
prop_2e_6 = unitTest
            "sort (inputs (simplify example)) == [\"x\",\"y\",\"z\"]"
            (sort (inputs (simplify example)) == ["x","y","z"])
            True

-- Tests Part 3
-- ----------------------------------------------------------------------------

-- 3a
prop_3a_1 :: Test
prop_3a_1 = unitTest
            "simulate (cinput \"x\") [(\"x\",True)] == True"
            (simulate (cinput "x") [("x",True)] == True)
            True

prop_3a_2 :: Test
prop_3a_2 = unitTest
            "simulate (cinput \"x\") [(\"x\",False)] == False"
            (simulate (cinput "x") [("x",False)] == False)
            True

prop_3a_3 :: Test
prop_3a_3 = unitTest
            "simulate (cnot (cinput \"x\")) [(\"x\",True)] == False"
            (simulate (cnot (cinput "x")) [("x",True)] == False)
            True

prop_3a_4 :: Test
prop_3a_4 = unitTest
            "simulate (cnot (cinput \"x\")) [(\"x\",False)] == True"
            (simulate (cnot (cinput "x")) [("x",False)] == True)
            True

prop_3a_5 :: Test
prop_3a_5 = unitTest
            "simulate (cand  (cinput \"x\") (cinput \"y\")) [(\"x\",True),(\"y\",True)] == True"
            (simulate (cand (cinput "x") (cinput "y")) [("x",True),("y",True)] == True)
            True

prop_3a_6 :: Test
prop_3a_6 = unitTest
            "simulate (cand  (cinput \"x\") (cinput \"y\")) [(\"y\",True),(\"x\",True)] == True"
            (simulate (cand (cinput "x") (cinput "y")) [("y",True),("x",True)] == True)
            True

-- 3b
prop_3b_1 :: Test
prop_3b_1 = unitTest
            "combinations 0 == [[]]"
            (combinations 0 == [[]])
            True

prop_3b_2 :: Test
prop_3b_2 = unitTest
            "combinations 1 == [[False],[True]]"
            (combinations 1 == [[False],[True]])
            True

prop_3b_3 :: Test
prop_3b_3 = unitTest
            "combinations 2 == [[False,False],[False,True],[True,False],[True,True]]"
            (combinations 2 == [[False,False],[False,True],[True,False],[True,True]])
            True



-- All the tests to run
allTests :: [Test]
allTests = [ prop_1b2a_1, prop_1b2a_2, prop_1b2a_3
           , prop_1b2a_4, prop_1b2a_5
           , prop_1c2a_1
           , prop_1d2a_1
           , prop_2b2a_1, prop_2b2a_2, prop_2b2a_3
           , prop_2b2a_4
           , prop_2c_1, prop_2c_2, prop_2c_3
           , prop_2c_4, prop_2c_5, prop_2c_6
           , prop_2c_7
           , prop_2d_1, prop_2d_2, prop_2d_3
           , prop_2d_4, prop_2d_5, prop_2d_6
           , prop_2d_7
           , prop_2e_1, prop_2e_2, prop_2e_3
           , prop_2e_4, prop_2e_5, prop_2e_6
           , prop_3a_1, prop_3a_2, prop_3a_3
           , prop_3a_4, prop_3a_5, prop_3a_6
           , prop_3b_1, prop_3b_2, prop_3b_3
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

