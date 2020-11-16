
module Main (main) where

import Template
import Testing

allTests :: [Test]
allTests = [
             unitTest "idTree (Leaf 1) == Leaf 1"
             ( idTree (Leaf 1) )
             (Leaf 1)
           , unitTest "idTree (Fork (Leaf 1) (Leaf 2)) == Fork (Leaf 1) (Leaf 2)"
             ( idTree (Fork (Leaf 1) (Leaf 2)) )
             (Fork (Leaf 1) (Leaf 2))
           , unitTest "idTree (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) == Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))"
             ( idTree (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) )
             ( Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3)) )

           , unitTest "nrOfLeaves (Leaf 1) == 1"
             ( nrOfLeaves (Leaf 1) )
             1
           , unitTest "nrOfLeaves (Fork (Leaf 1) (Leaf 2)) == 2"
             ( nrOfLeaves (Fork (Leaf 1) (Leaf 2)) )
             2
           , unitTest "nrOfLeaves (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) == 3"
             ( nrOfLeaves (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) )
             3

           , unitTest "sumTree (Leaf 1) == 1"
             ( sumTree (Leaf 1) )
             1
           , unitTest "sumTree (Fork (Leaf 1) (Leaf 2)) == 3"
             ( sumTree (Fork (Leaf 1) (Leaf 2)) )
             3
           , unitTest "sumTree (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) == 6"
             ( sumTree (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) )
             6

           , unitTest "depthOfTree (Leaf 1) == 1"
             ( depthOfTree (Leaf 1) )
             1
           , unitTest "depthOfTree (Fork (Leaf 1) (Leaf 2)) == 2"
             ( depthOfTree (Fork (Leaf 1) (Leaf 2)) )
             2
           , unitTest "depthOfTree (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) == 3"
             ( depthOfTree (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) )
             3

           , unitTest "treeToList (Leaf 1) == [1]"
             ( treeToList (Leaf 1) )
             [1]
           , unitTest "treeToList (Fork (Leaf 1) (Leaf 2)) == [1, 2]"
             ( treeToList (Fork (Leaf 1) (Leaf 2)) )
             [1,2]
           , unitTest "treeToList (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) == [1,2,3]"
             ( treeToList (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) )
             [1,2,3]

           , unitTest "minTree (Leaf 1) == 1"
             ( minTree (Leaf 1) )
             1
           , unitTest "minTree (Fork (Leaf 1) (Leaf 2)) == 1"
             ( minTree (Fork (Leaf 1) (Leaf 2)) )
             1
           , unitTest "minTree (Fork (Leaf 5) (Leaf 4)) == 4"
             ( minTree (Fork (Leaf 5) (Leaf 4)) )
             4
           , unitTest "minTree (Fork (Leaf 10) (Fork (Leaf 20) (Leaf 30))) == 10"
             ( minTree (Fork (Leaf 10) (Fork (Leaf 20) (Leaf 30))) )
             10
           , unitTest "minTree (Fork (Fork (Leaf 20) (Leaf 30)) (Leaf 10) ) == 10"
             ( minTree (Fork (Fork (Leaf 20) (Leaf 30)) (Leaf 10) ) )
             10

           , unitTest "mirrorTree (Leaf 1) == Leaf 1"
             ( mirrorTree (Leaf 1) )
             (Leaf 1)
           , unitTest "mirrorTree (Fork (Leaf 1) (Leaf 2)) == Fork (Leaf 2) (Leaf 1)"
             ( mirrorTree (Fork (Leaf 1) (Leaf 2)) )
             (Fork (Leaf 2) (Leaf 1))
           , unitTest "mirrorTree (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) == Fork (Fork (Leaf 3) (Leaf 2)) (Leaf 1) "
             ( mirrorTree (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) )
             ( Fork (Fork (Leaf 3) (Leaf 2)) (Leaf 1) )

           , unitTest "addOne (Leaf 1) == Leaf 2"
             ( addOne (Leaf 1) )
             (Leaf 2)
           , unitTest "addOne (Fork (Leaf 1) (Leaf 2)) == Fork (Leaf 2) (Leaf 3)"
             ( addOne (Fork (Leaf 1) (Leaf 2)) )
             (Fork (Leaf 2) (Leaf 3))
           , unitTest "addOne (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) == Fork (Leaf 2) (Fork (Leaf 3) (Leaf 4))"
             ( addOne (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) )
             ( Fork (Leaf 2) (Fork (Leaf 3) (Leaf 4)) )
           ]

-- Default call
main :: IO ()
main = processSubmission allTests
