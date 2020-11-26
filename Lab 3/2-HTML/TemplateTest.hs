{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Template
import Testing

-- ----------------------------------------------------------------------------

-- Test for HTML instance Link 
-- ----------------------------------------------------------------------------
prop_link :: Test 
prop_link = unitTest 
            "toHtml (Link \"https://www.kuleuven.be/kuleuven/\" \"KU Leuven\")"
            (toHtml (Link "https://www.kuleuven.be/kuleuven/" "KU Leuven"))
            (example :: HtmlElement)

prop_ul :: Test 
prop_ul = unitTest
          "exampleUL"
          (exampleUL :: HtmlElement)
          (HtmlTag "ul" [] [HtmlTag "li" [] [HtmlString "Apples"],HtmlTag "li" [] [HtmlString "Bananas"],HtmlTag "li" [] [HtmlString "Oranges"]])

-- All the tests to run
allTests :: [Test]
allTests = [
             prop_link,
             prop_ul
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

