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

-- * Prolog terms unit tests
-- ----------------------------------------------------------------------------

prop_term_1 :: Test
prop_term_1 = unitTest
  "occurs 1 (F \"f\" [])"
  (occurs 1 (F "f" []))
  False

prop_term_2 :: Test
prop_term_2 = unitTest
  "occurs 1 (F \"f\" [Var 1])"
  (occurs 1 (F "f" [Var 1]))
  True

prop_term_3 :: Test
prop_term_3 = unitTest
  "occurs 2 (F \"f\" [F \"g\" [Var 2], F \"h\" [Var 1]]"
  (occurs 2 (F "f" [F "g" [Var 2], F "h" [Var 1]]))
  True

-- * Substitution unit tests
-- ----------------------------------------------------------------------------

prop_substitution_1 :: Test
prop_substitution_1 = randomTest
  "forall n m. (n /= m) ==> applySubst [(n,F \"f\" [])] (Var m) == Var m"
  2
  (\n m -> (n /= m) ==> applySubst [(n,F "f" [])] (Var m) == Var m)

prop_substitution_2 :: Test
prop_substitution_2 = unitTest
  "applySubst [(0,F \"g\" [])] (Var 0)"
  (applySubst [(0,F "g" [])] (Var 0))
  (F "g" [])

prop_substitution_6 :: Test
prop_substitution_6 = unitTest
  "applySubst [(0,F \"g\" [])] (Var 1)"
  (applySubst [(0,F "g" [])] (Var 1))
  (Var 1)

prop_substitution_3 :: Test
prop_substitution_3 = unitTest
  "applySubst [(0,F \"g\" [Var 1])] (F \"f\" [Var 0])"
  (applySubst [(0,F "g" [Var 1])] (F "f" [Var 0]))
  (F "f" [F "g" [Var 1]])

prop_substitution_4 :: Test
prop_substitution_4 = unitTest
  "applySubst [(0,F \"g\" [Var 1])] (F \"f\" [F \"g\" [Var 1]])"
  (applySubst [(0,F "g" [Var 1])] (F "f" [F "g" [Var 1]]))
  (F "f" [F "g" [Var 1]])

prop_substitution_5 :: Test
prop_substitution_5 = unitTest
  "applySubst [(0,F \"g\" []),(1,Var 2)] (F \"f\" [F \"g\" [Var 1], Var 0])"
  (applySubst [(0,F "g" []),(1,Var 2)] (F "f" [F "g" [Var 1], Var 0]))
  (F "f" [F "g" [Var 2],F "g" []])

prop_conc_1 :: Test
prop_conc_1 = unitTest
  "conc [(0,F \"f\" [Var 1])] [(1,F \"g\" [Var 0]),(2,Var 1)]"
  (conc [(0,F "f" [Var 1])] [(1,F "g" [Var 0]),(2,Var 1)])
  [(0,F "f" [Var 1]),(1,F "g" [F "f" [Var 1]]),(2,Var 1)]

prop_conc_2 :: Test
prop_conc_2 = unitTest
  "conc [] [(1, F \"g\" [Var 2])]"
   (conc [] [(1, F "g" [Var 2])])
   [(1,F "g" [Var 2])]

-- * Unification unit tests
-- ----------------------------------------------------------------------------

prop_unify_1 :: Test
prop_unify_1 = unitTest
  "unify1 (Var 0) (F \"f\" [])"
  (unify1 (Var 0) (F "f" []))
  (Just [(0,F "f" [])])

prop_unify_2 :: Test
prop_unify_2 = unitTest
  "unify1 (Var 0) (F \"f\" [])"
  (unify1 (Var 0) (F "f" []))
  (Just [(0,F "f" [])])

prop_unify_3 :: Test
prop_unify_3 = unitTest
  "unify1 (F \"f\" []) (Var 0)"
  (unify1 (F "f" []) (Var 0))
  (Just [(0,F "f" [])])

prop_unify_4 :: Test
prop_unify_4 = unitTest
  "unify1 (F \"f\" [F \"g\" [Var 1]]) (F \"f\" [Var 0])"
  (unify1 (F "f" [F "g" [Var 1]]) (F "f" [Var 0]))
  (Just [(0,F "g" [Var 1])])

prop_unify_6 :: Test
prop_unify_6 = unitTest
  "unify1 (F \"g\" []) (F \"f\" [])"
  (unify1 (F "g" []) (F "f" []))
  Nothing

prop_unify_8 :: Test
prop_unify_8 = unitTest
  "unify1 (F \"f\" [Var 0, Var 1]) (F \"f\" [Var 0])"
  (unify1 (F "f" [Var 0, Var 1]) (F "f" [Var 0]))
  Nothing

prop_unify_7 :: Test
prop_unify_7 = unitTest
  "unify [(F \"f\" [Var 1],Var 0),(F \"g\" [Var 1],F \"g\" [F \"a\" []])]"
  (unify [(F "f" [Var 1],Var 0),(F "g" [Var 1],F "g" [F "a" []])])
  (Just [(1,F "a" []),(0,F "f" [F "a" []])])

allTests :: [Test]
allTests = [ prop_term_1, prop_term_2, prop_term_3
           , prop_substitution_1, prop_substitution_2, prop_substitution_3, prop_substitution_4, prop_substitution_5, prop_substitution_6
           , prop_conc_1, prop_conc_2
           , prop_unify_1, prop_unify_2, prop_unify_3, prop_unify_4, prop_unify_6, prop_unify_7, prop_unify_8
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
