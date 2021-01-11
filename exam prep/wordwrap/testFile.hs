module MyHaskellTest where

import MyHaskell
import Testing

corporateIpsum :: String
corporateIpsum = "Leverage agile frameworks to provide a robust synopsis for high level overviews. Iterative approaches to corporate strategy foster collaborative thinking to further the overall value proposition. Organically grow the holistic world view of disruptive innovation via workplace diversity and empowerment."

corporateIpsum50 :: String
corporateIpsum50 = "Leverage agile frameworks to provide a robust\nsynopsis for high level overviews. Iterative\napproaches to corporate strategy foster\ncollaborative thinking to further the overall\nvalue proposition. Organically grow the holistic\nworld view of disruptive innovation via workplace\ndiversity and empowerment."

corporateIpsum32 :: String
corporateIpsum32 = "Leverage agile frameworks to\nprovide a robust synopsis for\nhigh level overviews. Iterative\napproaches to corporate strategy\nfoster collaborative thinking to\nfurther the overall value\nproposition. Organically grow\nthe holistic world view of\ndisruptive innovation via\nworkplace diversity and\nempowerment."


test_1 :: Test
test_1 = unitTest
         "1.2 & 1.3 show mkSpace"
         (show mkSpace)
         "\" \""

test_2 :: Test
test_2 = unitTest
         "1.2 & 1.3 show mkNewLine"
         (show mkNewline)
         "\"\\n\""

test_3 :: Test
test_3 = unitTest
         "1.2 & 1.3 show (mkWord \"Hello\")"
         (show (mkWord "Hello"))
         "\"Hello\""


test_4 :: Test
test_4 = unitTest
         "1.4 length (toLineItems \"See ya, John.\")"
         (length (toLineItems "See ya, John."))
         5

test_5 :: Test
test_5 = unitTest
         "1.4 length (toLineItems \"Hint: read the\\nassignment carefully!\")"
         (length (toLineItems "Hint: read the\nassignment carefully!"))
         9

test_6 :: Test
test_6 = unitTest
         "1.4 length (toLineItems \"! oops  \\n\\n \")"
         (length (toLineItems "! oops  \n\n "))
         8


test_7 :: Test
test_7 = unitTest
         "1.5 fromLineItems (toLineItems \"See ya, John.\")"
         (fromLineItems (toLineItems "See ya, John."))
         "See ya, John."

test_8 :: Test
test_8 = unitTest
         "1.5 fromLineItems (toLineItems \"Hint: read the\\nassignment carefully!\")"
         (fromLineItems (toLineItems "Hint: read the\nassignment carefully!"))
         "Hint: read the\nassignment carefully!"

test_9 :: Test
test_9 = unitTest
         "1.5 fromLineItems (toLineItems \"! oops  \\n\\n \")"
         (fromLineItems (toLineItems "! oops  \n\n "))
         "! oops  \n\n "


test_10 :: Test
test_10 = unitTest
         "2.1 removeSpaces [mkWord \"hello\", mkSpace, mkWord \"world\", mkNewline, mkWord \"Bye\", mkSpace]"
         (show $ removeSpaces [mkWord "hello", mkSpace, mkWord "world", mkNewline, mkWord "Bye", mkSpace])
         "[\"hello\",\"world\",\"\\n\",\"Bye\"]"

test_11 :: Test
test_11 = unitTest
         "2.1 removeSpaces [mkWord \"hi\", mkSpace, mkSpace, mkNewline, mkSpace, mkNewline, mkSpace]"
         (show $ removeSpaces [mkWord "hi", mkSpace, mkSpace, mkNewline, mkSpace, mkNewline, mkSpace])
         "[\"hi\",\"\\n\",\"\\n\"]"


test_12 :: Test
test_12 = unitTest
         "2.2 splitInLines [mkWord \"hi\", mkNewline, mkWord \"bye\"]"
         (show $ splitInLines [mkWord "hi", mkNewline, mkWord "bye"])
         "[[\"hi\"],[\"bye\"]]"

test_13 :: Test
test_13 = unitTest
         "2.2 splitInLines [mkNewline, mkWord \"hi\", mkNewline, mkNewline, mkWord \"bye\", mkNewline]"
         (show $ splitInLines [mkNewline, mkWord "hi", mkNewline, mkNewline, mkWord "bye", mkNewline])
         "[[],[\"hi\"],[],[\"bye\"],[]]"

test_14 :: Test
test_14 = unitTest
         "2.2 splitInLines []"
         (splitInLines [])
         [[]]

test_15 :: Test
test_15 = unitTest
         "2.2 splitInLines [mkNewline]"
         (splitInLines [mkNewline])
         [[], []]

test_16 :: Test
test_16 = unitTest
         "2.2 splitInLines [mkNewline, mkNewline]"
         (splitInLines [mkNewline, mkNewline])
         [[], [], []]

test_17 :: Test
test_17 = unitTest
         "2.2 splitInLines [mkWord \"foo\", mkNewline]"
         (show $ splitInLines [mkWord "foo", mkNewline])
         "[[\"foo\"],[]]"


test_18 :: Test
test_18 = unitTest
         "2.3 separateTooLongWords 6 [mkWord \"look\", mkWord \"a\", mkWord \"brontosaurus\", mkWord \"there\"]"
         (show $ separateTooLongWords 6 [mkWord "look", mkWord "a", mkWord "brontosaurus", mkWord "over", mkWord "there"])
         "[[\"look\",\"a\"],[\"brontosaurus\"],[\"over\",\"there\"]]"

test_19 :: Test
test_19 = unitTest
         "2.3 separateTooLongWords 3 [mkWord \"Yuuuuge\", mkWord \"amazing\"]"
         (show $ separateTooLongWords 3 [mkWord "Yuuuuge", mkWord "amazing"])
         "[[\"Yuuuuge\"],[\"amazing\"]]"

test_20 :: Test
test_20 = unitTest
         "2.3 separateTooLongWords 3 [mkWord \"Banana\"]"
         (show $ separateTooLongWords 3 [mkWord "Banana"])
         "[[\"Banana\"]]"

test_21 :: Test
test_21 = unitTest
         "2.3 separateTooLongWords 100 [mkWord \"Banana\"]"
         (show $ separateTooLongWords 100 [mkWord "Banana"])
         "[[\"Banana\"]]"


test_22 :: Test
test_22 = unitTest
         "2.4 wrap 7 [mkWord \"foo\", mkWord \"bar\", mkWord \"qu\", mkWord \"u\", mkWord \"x\", mkWord \"banana\"]"
         (show $ wrap 7 [mkWord "foo", mkWord "bar", mkWord "qu", mkWord "u", mkWord "x", mkWord "banana"])
         "[[\"foo\",\"bar\"],[\"qu\",\"u\",\"x\"],[\"banana\"]]"

test_23 :: Test
test_23 = unitTest
         "2.4 wrap 4 [mkWord \"gyronef\"]"
         (show $ wrap 4 [mkWord "gyronef"])
         "[[\"gyronef\"]]"


test_24 :: Test
test_24 = unitTest
         "2.5 joinLineWithSpaces [mkWord \"so\", mkWord \"much\", mkWord \"space\"]"
         (show $ joinLineWithSpaces [mkWord "so", mkWord "much", mkWord "space"])
         "[\"so\",\" \",\"much\",\" \",\"space\"]"


test_25 :: Test
test_25 = unitTest
         "2.6 joinLinesWithNewlines [[mkWord \"hi\", mkSpace, mkWord \"there\"],[mkWord \"bye\"]]"
         (show $ joinLinesWithNewlines [[mkWord "hi", mkSpace, mkWord "there"],[mkWord "bye"]])
         "[\"hi\",\" \",\"there\",\"\\n\",\"bye\"]"


test_26 :: Test
test_26 = unitTest
         "wordWrap 5 \"a b c d e f g\""
         (wordWrap 5 "a b c d e f g")
         "a b c\nd e f\ng"

test_27 :: Test
test_27 = unitTest
         "wordWrap 4 \"a b c d e f g\""
         (wordWrap 4 "a b c d e f g")
         "a b\nc d\ne f\ng"

test_28 :: Test
test_28 = unitTest
         "wordWrap 5 \"a\\nb c d e f g\""
         (wordWrap 5 "a\nb c d e f g")
         "a\nb c d\ne f g"

test_29 :: Test
test_29 = unitTest
         "wordWrap 4 \"\\n food\""
         (wordWrap 4 "\n food")
         "\nfood"

test_30 :: Test
test_30 = unitTest
         "wordWrap 4 \"a b c delta e f g\""
         (wordWrap 4 "a b c delta e f g")
         "a b\nc\ndelta\ne f\ng"

test_31 :: Test
test_31 = unitTest
         "wordWrap 7 \"foo  bar  \""
         (wordWrap 7 "foo  bar  ")
         "foo bar"

test_32 :: Test
test_32 = unitTest
         "wordWrap 20 \"  foo  \\n  \\n  bar  \""
         (wordWrap 20 "  foo  \n  \n  bar  ")
         "foo\n\nbar"

test_33 :: Test
test_33 = unitTest
         "wordWrap 50 corporateIpsum"
         (wordWrap 50 corporateIpsum)
         corporateIpsum50

test_34 :: Test
test_34 = unitTest
         "wordWrap 32 corporateIpsum"
         (wordWrap 32 corporateIpsum)
         corporateIpsum32

allTests :: [Test]
allTests = [ test_1, test_2, test_3
           , test_4, test_5, test_6
           , test_7, test_8, test_9
           , test_10, test_11, test_12
           , test_13, test_14, test_15
           , test_16, test_17, test_18
           , test_19, test_20, test_21
           , test_22, test_23, test_24
           , test_25, test_26, test_27
           , test_28, test_29, test_30
           , test_31, test_32, test_33
           , test_34
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
