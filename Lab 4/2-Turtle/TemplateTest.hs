{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Prelude hiding (GT)
import Template
import Testing
  
prop_1 :: Test
prop_1 = unitTest
  "boundingBox (turtleToLines square)"
  (boundingBox (turtleToLines square))
  ((500.0,500.0),(550.0,550.0))

prop_3 :: Test
prop_3 = unitTest
  "animate square"
  (animate square)
  [Step 50.0 Done,Step 50.0 (Turn 90.0 (Step 50.0 Done)),Step 50.0 (Turn 90.0 (Step 50.0 (Turn 90.0 (Step 50.0 Done)))),Step 50.0 (Turn 90.0 (Step 50.0 (Turn 90.0 (Step 50.0 (Turn 90.0 (Step 50.0 Done))))))]

prop_5 :: Test
prop_5 = unitTest
  "turtleToLines xi"
  (turtleToLines xi)
  [((500.0,500.0),(550.0,500.0)),((540.0,485.0),(510.0,485.0)), ((500.0,470.0),(550.0,470.0))]

prop_6 :: Test
prop_6 = unitTest
  "length (animate xi)"
  (length (animate xi))
  3

prop_7 :: Test
prop_7 = unitTest
  "length (animate square)"
  (length (animate square))
  4

prop_8 :: Test
prop_8 = unitTest
  "dashedStep 3 10"
  (dashedStep 3 10)
  (PenDown (Step 2.0 (PenUp (Step 2.0 (PenDown (Step 2.0 (PenUp (Step 2.0 (PenDown (Step 2.0 Done))))))))))

prop_9 :: Test
prop_9 = unitTest
  "dash 2 xi"
  (dash 2 xi)
  (Turn 90.0 (PenDown (Step 16.666666666666668 (PenUp (Step 16.666666666666668 (PenDown (Step 16.666666666666668 (Turn 90.0 (PenUp (Step 15.0 (Turn 90.0 (Step 10.0 (PenDown (PenDown (Step 10.0 (PenUp (Step 10.0 (PenDown (Step 10.0 (PenUp (Step 10.0 (Turn (-90.0) (Step 15.0 (Turn (-90.0) (PenDown (PenDown (Step 16.666666666666668 (PenUp (Step 16.666666666666668 (PenDown (Step 16.666666666666668 Done)))))))))))))))))))))))))))))))

prop_10 :: Test
prop_10 = unitTest
  "compileFractal' (dragon L) dragon 1 10.0"
  (compileFractal' (dragon L) dragon 1 10.0)
  (Turn 45.0 (Turn 45.0 (Step 10.0 (Turn (-90.0) (Step 10.0 (Turn 45.0 (Turn (-90.0) (Turn (-45.0) (Step 10.0 (Turn 90.0 (Step 10.0 (Turn (-45.0) (Turn 45.0 Done)))))))))))))

prop_11 :: Test
prop_11 = unitTest
  "compileFractal' (dragon L) dragon 2 10.0"
  (compileFractal' (dragon L) dragon 2 10.0)
  (Turn 45.0 (Turn 45.0 (Turn 45.0 (Step 10.0 (Turn (-90.0) (Step 10.0 (Turn 45.0 (Turn (-90.0) (Turn (-45.0) (Step 10.0 (Turn 90.0 (Step 10.0 (Turn (-45.0) (Turn 45.0 (Turn (-90.0) (Turn (-45.0) (Turn 45.0 (Step 10.0 (Turn (-90.0) (Step 10.0 (Turn 45.0 (Turn 90.0 (Turn (-45.0) (Step 10.0 (Turn 90.0 (Step 10.0 (Turn (-45.0) (Turn (-45.0) (Turn 45.0 Done)))))))))))))))))))))))))))))

prop_12 :: Test
prop_12 = unitTest
  "compileFractal' (dragon L) dragon 0 10.0"
  (compileFractal' (dragon L) dragon 0 10.0)
  (Turn 45.0 (Step 10.0 (Turn (-90.0) (Step 10.0 (Turn 45.0 Done)))))
  

allTests = [ prop_1, prop_3
           , prop_5
           , prop_6, prop_7, prop_8, prop_9
           , prop_10, prop_11, prop_12
           ]

main :: IO ()
main = runAndPrintTestsLocally True allTests
