
module Main (main) where

import Template
import Testing

knownSolution :: [Int] -> Int -> [[Int]]
knownSolution _ 0  = [[]]
knownSolution [] _ = []
knownSolution (j:js) i
  | j > i = knownSolution js i
  | otherwise =
    [ j:xs | xs <- knownSolution (j:js) (i - j)] ++ knownSolution js i
    
allTests :: [Test]
allTests = [
             unitTest "changesEuro 1 = [[1]]"
             ( changesEuro 1 )
             [[1]]
           , unitTest "length $ changesEuro 10 = 11"
             ( length $ changesEuro 10 )
             11
           , unitTest "length $ changesEuro 50 = 451"
             ( length $ changesEuro 50 )
             451
           , unitTest "changesEuro 0 = [[]]"
             ( changesEuro 0 )
             [[]]
           , unitTest "changes [] 10 = []"
             ( changes [] 10 )
             []
           , randomTest "forall n. (n < 50) ==> (length $ changesEuro n) == (length $ changesEuroRev n)"
             1
             (\n -> (n < 50) ==> (length $ changesEuro n) == (length $ changesEuroRev n))
           , randomTest "forall n. (n < 50) ==> (length $ knownSolution amountsEuro n) == (length $ changes amountsEuro n)"
             1
             (\n -> (n < 50) ==> (length $ knownSolution amountsEuro n) == (length $ changes amountsEuro n))
           ]

-- Default call
main :: IO ()
main = processSubmission allTests
