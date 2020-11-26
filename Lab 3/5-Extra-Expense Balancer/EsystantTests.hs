{-# OPTIONS_GHC -Wall                    #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

module Main (main) where
import Myhaskell
import Testing
import Control.Monad
import Control.Applicative
import Data.Maybe (isJust, isNothing)
prop_1_1 :: Test
prop_1_1 = unitTest
  "mkExpense \"Alex\" 10.0"
  (show(mkExpense "Alex" 10.0))
  "Alex: 10.0"
prop_1_2 :: Test
prop_1_2 = unitTest
  "mkExpense \"Alex\" 200.05"
  (show(mkExpense "Alex" 200.05))
  "Alex: 200.05"
prop_1_3 :: Test
prop_1_3 = unitTest
  "mkExpense \"Zorg\" 10 < mkExpense \"Alex\" 200.05"
  (show(mkExpense "Zorg" 10 < mkExpense "Alex" 200.05))
  "True"
prop_2_1 :: Test
prop_2_1 = unitTest
  "fromExpense 10 (mkExpense \"Alex\" 20)"
  (show(fromExpense 10 (mkExpense "Alex" 20)))
  "Alex: 10.0"
prop_2_2 :: Test
prop_2_2 = unitTest
  "fromExpense 50 (mkExpense \"Alex\" 0)"
  (show(fromExpense 50 (mkExpense "Alex" 0)))
  "Alex: -50.0"
prop_2_3 :: Test
prop_2_3 = unitTest
  "mkDelta \"Xander\" 10 < mkDelta \"Alex\" 200.05"
  (show(mkDelta "Xander" 10 < mkDelta "Alex" 200.05))
  "True"
prop_3_1 :: Test
prop_3_1 = unitTest
  "toDeltas [mkExpense \"Alex\" 40, mkExpense \"Gert-Jan\" 200]"
  (show(toDeltas [mkExpense "Alex" 40, mkExpense "Gert-Jan" 200]))
  "[Alex: -80.0,Gert-Jan: 80.0]"
prop_3_2 :: Test
prop_3_2 = unitTest
  "toDeltas [mkExpense \"Matthias\" 11.5, mkExpense \"Thomas\" 100]"
  (show(toDeltas [mkExpense "Matthias" 11.5, mkExpense "Thomas" 100]))
  "[Matthias: -44.25,Thomas: 44.25]"
prop_3_3 :: Test
prop_3_3 = unitTest
  "toDeltas [mkExpense \"Alex\" 11.5, mkExpense \"Gert-Jan\" 100, mkExpense \"Tom\" 1000]"
  (show(toDeltas [mkExpense "Alex" 11.5, mkExpense "Gert-Jan" 100, mkExpense "Tom" 1000]))
  "[Alex: -359.0,Gert-Jan: -270.5,Tom: 629.5]"
prop_4_1 :: Test
prop_4_1 = unitTest
  "applyTransfer (MkTransfer \"Alex\" \"Tom\" 100) (mkExpense \"Alex\" 125)"
  (show(applyTransfer (MkTransfer "Alex" "Tom" 100) (mkExpense "Alex" 125)))
  "Alex: 225.0"
prop_4_2 :: Test
prop_4_2 = unitTest
  "applyTransfer (MkTransfer \"Alex\" \"Tom\" 100) (mkExpense \"Tom\" 125)"
  (show(applyTransfer (MkTransfer "Alex" "Tom" 100) (mkExpense "Tom" 125)))
  "Tom: 25.0"
prop_4_3 :: Test
prop_4_3 = unitTest
  "applyTransfer (MkTransfer \"Alex\" \"Tom\" 100) (mkExpense \"Thomas\" 125)"
  (show(applyTransfer (MkTransfer "Alex" "Tom" 100) (mkExpense "Thomas" 125)))
  "Thomas: 125.0"
prop_4_4 :: Test
prop_4_4 = unitTest
  "applyTransfer (MkTransfer \"Tom\" \"Tom\" 100) (mkExpense \"Tom\" 125)"
  (show(applyTransfer (MkTransfer "Tom" "Tom" 100) (mkExpense "Tom" 125)))
  "Tom: 125.0"
prop_5_1 :: Test
prop_5_1 = unitTest
  "applyTransfer (MkTransfer \"Alex\" \"Tom\" 100) (mkDelta \"Alex\" 125)"
  (show(applyTransfer (MkTransfer "Alex" "Tom" 100) (mkDelta "Alex" 125)))
  "Alex: 225.0"
prop_5_2 :: Test
prop_5_2 = unitTest
  "applyTransfer (MkTransfer \"Alex\" \"Tom\" 100) (mkDelta \"Tom\" 125)"
  (show(applyTransfer (MkTransfer "Alex" "Tom" 100) (mkDelta "Tom" 125)))
  "Tom: 25.0"
prop_5_3 :: Test
prop_5_3 = unitTest
  "applyTransfer (MkTransfer \"Alex\" \"Tom\" 100) (mkDelta  \"Thomas\" 125)"
  (show(applyTransfer (MkTransfer "Alex" "Tom" 100) (mkDelta  "Thomas" 125)))
  "Thomas: 125.0"
prop_5_4 :: Test
prop_5_4 = unitTest
  "applyTransfer (MkTransfer \"Tom\" \"Tom\" 100) (mkDelta  \"Tom\" 125)"
  (show(applyTransfer (MkTransfer "Tom" "Tom" 100) (mkDelta  "Tom" 125)))
  "Tom: 125.0"
prop_6_1 :: Test
prop_6_1 = unitTest
  "createTransfer 100 (mkDelta \"Alex\" (-100)) (mkDelta \"Tom\" 200)"
  (show(createTransfer 100 (mkDelta "Alex" (-100)) (mkDelta "Tom" 200)))
  "Alex -> Tom:100.0"
prop_6_2 :: Test
prop_6_2 = unitTest
  "createTransfer 30 (mkDelta \"Tom\" (-100)) (mkDelta \"Tom\" 200)"
  (show(createTransfer 30 (mkDelta "Tom" (-100)) (mkDelta "Tom" 200)))
  "Tom -> Tom:30.0"
prop_7_1 :: Test
prop_7_1 = unitTest
  "balanced [mkExpense \"Alex\" 100,mkExpense \"Matthias\" 125.0] 0.01"
  (show(balanced [mkExpense "Alex" 100,mkExpense "Matthias" 125.0] 0.01))
  "False"
prop_7_2 :: Test
prop_7_2 = unitTest
  "balanced [mkExpense \"Alex\" 100,mkExpense \"Matthias\" 100] 0.01"
  (show(balanced [mkExpense "Alex" 100,mkExpense "Matthias" 100] 0.01))
  "True"
prop_7_3 :: Test
prop_7_3 = unitTest
  "balanced [mkExpense \"Alex\" 100.5,mkExpense \"Matthias\" 100] 1"
  (show(balanced [mkExpense "Alex" 100.5,mkExpense "Matthias" 100] 1))
  "True"
prop_8_1 :: Test
prop_8_1 = unitTest
  "balanceDeltas [mkDelta \"Alex\" (-175), mkDelta \"Gert-Jan\" (-275), mkDelta \"Tom\" 425, mkDelta \"Thomas\" 25] 0.01"
  (show(balanceDeltas [mkDelta "Alex" (-175), mkDelta "Gert-Jan" (-275), mkDelta "Tom" 425, mkDelta "Thomas" 25] 0.01))
  "[Gert-Jan -> Tom:275.0,Alex -> Tom:150.0,Alex -> Thomas:25.0]"
prop_9_1 :: Test
prop_9_1 = unitTest
  "let expenses = [mkExpense \"Alex\" 200, mkExpense \"Gert-Jan\" 40, mkExpense \"Tom\" 1000,mkExpense \"Thomas\" 275.5] in balance expenses 0.01"
  (show(let expenses = [mkExpense "Alex" 200, mkExpense "Gert-Jan" 40, mkExpense "Tom" 1000,mkExpense "Thomas" 275.5] in balance expenses 0.01))
  "[Gert-Jan -> Tom:338.875,Alex -> Tom:178.875,Thomas -> Tom:103.375]"
allTests :: [Test]
allTests = [prop_1_1, prop_1_2, prop_1_3, prop_2_1, prop_2_2, prop_2_3, prop_3_1, prop_3_2, prop_3_3, prop_4_1, prop_4_2, prop_4_3, prop_4_4, prop_5_1, prop_5_2, prop_5_3, prop_5_4, prop_6_1, prop_6_2, prop_7_1, prop_7_2, prop_7_3, prop_8_1, prop_9_1]


-- Default call
main :: IO ()
main = processSubmission allTests
