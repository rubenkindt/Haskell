
module Main (main) where

import MyHaskell
import Testing

corporateIpsum :: String
corporateIpsum = "Leverage agile frameworks to provide a robust synopsis for high level overviews. Iterative approaches to corporate strategy foster collaborative thinking to further the overall value proposition. Organically grow the holistic world view of disruptive innovation via workplace diversity and empowerment."

corporateIpsum50 :: String
corporateIpsum50 = "Leverage agile frameworks to provide a robust\nsynopsis for high level overviews. Iterative\napproaches to corporate strategy foster\ncollaborative thinking to further the overall\nvalue proposition. Organically grow the holistic\nworld view of disruptive innovation via workplace\ndiversity and empowerment."

corporateIpsum32 :: String
corporateIpsum32 = "Leverage agile frameworks to\nprovide a robust synopsis for\nhigh level overviews. Iterative\napproaches to corporate strategy\nfoster collaborative thinking to\nfurther the overall value\nproposition. Organically grow\nthe holistic world view of\ndisruptive innovation via\nworkplace diversity and\nempowerment."

allTests :: [Test]
allTests = [
            unitTest "1.2 & 1.3 show mkSpace == \"\\\" \\\"\""
            (show mkSpace)
            ("\" \"")

          , unitTest "1.2 & 1.3 show mkSpace == \"\\\"\\\\n\\\"\""
              (show mkNewline)
              ("\"\\n\"")
          , unitTest "1.2 & 1.3 show (mkWord \"Hello\") == \"\\\"Hello\\\"\""
              (show (mkWord "Hello"))
              ("\"Hello\"")

          , unitTest "1.4 length (toLineItems \"See ya, John.\") == 5"
              (length (toLineItems "See ya, John."))
              (5)
          , unitTest "1.4 length (toLineItems \"Hint: read the\\nassignment carefully!\") == 9"
              (length (toLineItems "Hint: read the\nassignment carefully!"))
              (9)
          , unitTest "1.4 length (toLineItems \"! oops  \\n\\n \") == 8"
              (length (toLineItems "! oops  \n\n "))
              (8)

          , unitTest "1.5 fromLineItems (toLineItems \"See ya, John.\") == \"See ya, John.\""
              (fromLineItems (toLineItems "See ya, John."))
              ("See ya, John.")
          , unitTest "1.5 fromLineItems (toLineItems \"Hint: read the\\nassignment carefully!\") == \"Hint: read the\\nassignment carefully!\""
              (fromLineItems (toLineItems "Hint: read the\nassignment carefully!"))
              ("Hint: read the\nassignment carefully!")
          , unitTest "1.5 fromLineItems (toLineItems \"! oops  \\n\\n \") == \"! oops  \\n\\n \""
              (fromLineItems (toLineItems "! oops  \n\n "))
              ("! oops  \n\n ")

          , unitTest "2.1 removeSpaces [mkWord \"hello\", mkSpace, mkWord \"world\", mkNewline, mkWord \"Bye\", mkSpace] == [mkWord \"hello\", mkWord \"world\", mkNewline, mkWord \"Bye\"]"
              (removeSpaces [mkWord "hello", mkSpace, mkWord "world", mkNewline, mkWord "Bye", mkSpace])
              ([mkWord "hello", mkWord "world", mkNewline, mkWord "Bye"])
          , unitTest "2.1 removeSpaces [mkWord \"hi\", mkSpace, mkSpace, mkNewline, mkSpace, mkNewline, mkSpace] == [mkWord \"hi\", mkNewline, mkNewline]"
              (removeSpaces [mkWord "hi", mkSpace, mkSpace, mkNewline, mkSpace, mkNewline, mkSpace])
              ([mkWord "hi", mkNewline, mkNewline])

          , unitTest "2.2 splitInLines [mkWord \"hi\", mkNewline, mkWord \"bye\"] == [[mkWord \"hi\"], [mkWord \"bye\"]]"
              (splitInLines [mkWord "hi", mkNewline, mkWord "bye"])
              ([[mkWord "hi"], [mkWord "bye"]])
          , unitTest "2.2 splitInLines [mkNewline, mkWord \"hi\", mkNewline, mkNewline, mkWord \"bye\", mkNewline] == [[], [mkWord \"hi\"], [], [mkWord \"bye\"], []]"
              (splitInLines [mkNewline, mkWord "hi", mkNewline, mkNewline, mkWord "bye", mkNewline])
              ([[], [mkWord "hi"], [], [mkWord "bye"], []])
          , unitTest "2.2 splitInLines [] == [[]]"
              (splitInLines [])
              ([[]])
          , unitTest "2.2 splitInLines [mkNewline] == [[], []]"
              (splitInLines [mkNewline])
              ([[], []])
          , unitTest "2.2 splitInLines [mkNewline, mkNewline] == [[], [], []]"
              (splitInLines [mkNewline, mkNewline])
              ([[], [], []])
          , unitTest "2.2 splitInLines [mkWord \"foo\", mkNewline] == [[mkWord \"foo\"], []]"
              (splitInLines [mkWord "foo", mkNewline])
              ([[mkWord "foo"], []])

          , unitTest "2.3 separateTooLongWords 6 [mkWord \"look\", mkWord \"a\", mkWord \"brontosaurus\", mkWord \"there\"] == [[\"look\",\"a\"],[\"brontosaurus\"],[\"there\"]]"
              (separateTooLongWords 6 [mkWord "look", mkWord "a", mkWord "brontosaurus", mkWord "over", mkWord "there"])
              ([[mkWord "look", mkWord "a"], [mkWord "brontosaurus"], [mkWord "over", mkWord "there"]])
          , unitTest "2.3 separateTooLongWords 3 [mkWord \"Yuuuuge\", mkWord \"amazing\"] == [[\"Yuuuuge\"],[\"amazing\"]]"
              (separateTooLongWords 3 [mkWord "Yuuuuge", mkWord "amazing"])
              ([[mkWord "Yuuuuge"], [mkWord "amazing"]])
          , unitTest "2.3 separateTooLongWords 3 [mkWord \"Banana\"] == [[mkWord \"Banana\"]]"
              (separateTooLongWords 3 [mkWord "Banana"])
              ([[mkWord "Banana"]])
          , unitTest "2.3 separateTooLongWords 100 [mkWord \"Banana\"] == [[mkWord \"Banana\"]]"
              (separateTooLongWords 100 [mkWord "Banana"])
              ([[mkWord "Banana"]])

          , unitTest "2.4 wrap 7 [mkWord \"foo\", mkWord \"bar\", mkWord \"qu\", mkWord \"u\", mkWord \"x\", mkWord \"banana\"] == [[mkWord \"foo\", mkWord \"bar\"], [mkWord \"qu\", mkWord \"u\", mkWord \"x\"], [mkWord \"banana\"]]"
              (wrap 7 [mkWord "foo", mkWord "bar", mkWord "qu", mkWord "u", mkWord "x", mkWord "banana"])
              ([[mkWord "foo", mkWord "bar"], [mkWord "qu", mkWord "u", mkWord "x"], [mkWord "banana"]])
          , unitTest "2.4 wrap 4 [mkWord \"gyronef\"] == [[\"gyronef\"]]"
              (wrap 4 [mkWord "gyronef"])
              ([[mkWord "gyronef"]])

          , unitTest "2.5 joinLineWithSpaces [mkWord \"so\", mkWord \"much\", mkWord \"space\"] == [mkWord \"so\", mkSpace, mkWord \"much\", mkSpace, mkWord \"space\"]"
              (joinLineWithSpaces [mkWord "so", mkWord "much", mkWord "space"])
              ([mkWord "so", mkSpace, mkWord "much", mkSpace, mkWord "space"])

          , unitTest "2.6 joinLinesWithNewlines [[mkWord \"hi\", mkSpace, mkWord \"there\"],[mkWord \"bye\"]] == [mkWord \"hi\", mkSpace, mkWord \"there\", mkNewline, mkWord \"bye\"]"
              (joinLinesWithNewlines [[mkWord "hi", mkSpace, mkWord "there"],[mkWord "bye"]])
              ([mkWord "hi", mkSpace, mkWord "there", mkNewline, mkWord "bye"])

          , unitTest "wordWrap 5 \"a b c d e f g\" == \"a b c\\nd e f\\ng\""
              (wordWrap 5 "a b c d e f g")
              ("a b c\nd e f\ng")
          , unitTest "wordWrap 4 \"a b c d e f g\" == \"a b\\nc d\\ne f\\ng\""
              (wordWrap 4 "a b c d e f g")
              ("a b\nc d\ne f\ng")
          , unitTest "wordWrap 5 \"a\\nb c d e f g\" == \"a\\nb c d\\ne f g\""
              (wordWrap 5 "a\nb c d e f g")
              ("a\nb c d\ne f g")
          , unitTest "wordWrap 4 \"\\n food\" == \"\\nfood\""
              (wordWrap 4 "\n food")
              ("\nfood")
          , unitTest "wordWrap 4 \"a b c delta e f g\" == \"a b\\nc\\ndelta\\ne f\\ng\""
              (wordWrap 4 "a b c delta e f g")
              ("a b\nc\ndelta\ne f\ng")
          , unitTest "wordWrap 7 \"foo  bar  \" == \"foo bar\""
              (wordWrap 7 "foo  bar  ")
              ("foo bar")
          , unitTest "wordWrap 20 \"  foo  \\n  \\n  bar  \" == \"foo\\n\\nbar\""
              (wordWrap 20 "  foo  \n  \n  bar  ")
              ("foo\n\nbar")
          , unitTest "wordWrap 50 corporateIpsum == corporateIpsum50"
              (wordWrap 50 corporateIpsum)
              (corporateIpsum50)
          , unitTest "wordWrap 32 corporateIpsum == corporateIpsum32"
              (wordWrap 32 corporateIpsum)
              (corporateIpsum32)
           ]

-- Default call
main :: IO ()
main = processSubmission allTests
