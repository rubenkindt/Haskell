{-# LANGUAGE GADTs                       #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Testing
--
-- Maintainer  :  george.karachalias@cs.kuleuven.be
--
-- Minimal DSL for testing.
--
-----------------------------------------------------------------------------

module Testing
( -- * Test and Result types
  Test, TestResult

  -- * Creating Tests
, unitTest
, randomTest
, unitTestIO

  -- * Running Tests And Printing The Results
, runTest, runTests
, runAndPrintTestsJSON
, runAndPrintTestsLocally
, processSubmission

  -- Re-export QuickCheck
, module Test.QuickCheck

-- * Usage Example
{- |

As an example, consider parts of the rock-paper-scissors game:

> module RPS where
>
> data Move = Rock | Paper | Scissors
>
> beat :: Move -> Move
> beat Rock     = Scissors
> beat Paper    = Paper
> beam Scissors = Scissors
>
> lose :: Move -> Move
> lose = undefined

A toy test file for the above could be the following:

> {-# LANGUAGE StandaloneDeriving #-}
>
> module RPSTest where
>
> import RPS (Move(..),beat,lose)
> import Testing
>
> -- * Some useful instances
> -- ---------------------------------------------------------------------
>
> -- Interface to students code. We are assuming that the student has kept the
> -- RPS order.
> deriving instance Bounded Move
> deriving instance Show Move
> deriving instance Enum Move
> deriving instance Eq Move
>
> instance Arbitrary Move where
>   arbitrary = arbitraryBoundedEnum
>
> allMoves :: [Move]
> allMoves = [minBound .. maxBound]
>
> -- * The tests
> -- ---------------------------------------------------------------------
>
> prop_beat_simple :: Test
> prop_beat_simple =
>   unitTest
>     "beat Paper /= Paper"
>     (beat Paper /= Paper)
>     True
>
> prop_move_valid :: Test
> prop_move_valid =
>   unitTest
>     "The Move datatype should have three constructors."
>     (length allMoves)
>     3
>
> prop_not_win_thyself :: Test
> prop_not_win_thyself =
>   randomTest
>     "forall m. beat m /= m"
>     1
>     (\m -> beat m /= m)
>
> prop_beatlose :: Test
> prop_beatlose =
>    randomTest
>      "beat . lose == id"
>      1
>      (\m -> beat (lose m) == m)
>
> prop_beat_injective :: Test
> prop_beat_injective =
>   randomTest
>     "forall m n. (beat m == beat n) ==> (m == n)"
>     2
>     (\m n -> (beat m == beat n) ==> (m == n))
>
> -- * Run the tests
> -- ---------------------------------------------------------------------
>
> allTests :: [Test]
> allTests = [ prop_beat_simple
>            , prop_move_valid
>            , prop_not_win_thyself
>            , prop_beatlose
>            , prop_beat_injective
>            ]
>
> -- -- Default call
> -- main :: IO ()
> -- main = runAndPrintTestsJSON allTests
>
> -- -- Uncomment this if you want to check the tests locally, without colors
> -- main :: IO ()
> -- main = runAndPrintTestsLocally False allTests
>
> -- Run the tests locally, with colors enabled
> main :: IO ()
> main = runAndPrintTestsLocally True allTests

If we run the above program (RPSTest.hs), we get the following result:

> Test 1 (unit test) : beat Paper /= Paper
>   expected result : "True"
>   actual result : "False"
>   status : "failed"
> Test 2 (unit test) : The Move datatype should have three constructors.
>   expected result : "3"
>   actual result : "3"
>   status : "passed"
> Test 3 (random test) : forall m. beat m /= m
>   status : "not-implemented"
>   counter-example : RPS.hs:(7,1)-(8,21): Non-exhaustive patterns in function beat
> Test 4 (random test) : beat . lose == id
>   status : "not-implemented"
>   counter-example : Prelude.undefined
> Test 5 (random test) : forall m n. (beat m == beat n) ==> (m == n)
>   status : "not-implemented"
>   counter-example : RPS.hs:(7,1)-(8,21): Non-exhaustive patterns in function beat

  * Test 1 fails because of 'beat's erroneous second clause:

> beat Paper = Paper:

  * Test 2 passes, since type 'Move' has 3 constructors as expected

  * Tests 3 and 5 are considered not to be implemented (everything that crashes
    is considered not-implemented) since there is a typo at the last clause of
    'beat' (it is called 'beam'):

> beam Scissors = Scissors

  * Test 4 is also considered not-implemented, since it crashes with error
    "Prelude.undefined", since function 'lose' is 'undefined'.
-}
) where

import Control.Exception
import Text.PrettyPrint
import Control.Applicative -- backwards compatible
import Control.Monad
import Data.List (intercalate)
import Data.Char (isSpace)
import Test.QuickCheck
import System.Console.GetOpt
import System.Environment
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types.Status (statusCode)
import System.Directory
import qualified Data.Text as T
import Data.Text.Encoding as TE
import Data.Ini
import GHC.IO.Handle
import System.Directory
import System.IO
import Control.DeepSeq



-- TODO: Add a note about the types exported by QuickCheck. There should not be
-- name collisions.

-- * Test Types And Test Results
-- ----------------------------------------------------------------------------

-- | The main test type. There are two kinds of tests, unit and random and a
-- test can be created using only the exported functions 'unitTest' and
-- 'randomTest' (see below).
data Test where
  UnitTest
    :: (Eq a, Show a) {- Should be printable and comparable for equality -}
    => String         {- Expression to evaluate (string representation)  -}
    -> a              {- Expression to evaluate                          -}
    -> a              {- Expected result                                 -}
    -> Test
  RandomTest
    :: Testable prop {- Should be testable in order to use QuickCheck    -}
    => String        {- Property to test (string representation)         -}
    -> Int           {- Number of property arguments                     -}
    -> prop          {- Property to test                                 -}
    -> Test
  UnitTestIO
    :: String       {- Expression to evaluate (string representation)  -}
    -> IO a         {- Expression to evaluate -}
    -> [String]     {- input for functions to test -}
    -> [String]     {- expected output each string represents a different line -}
    -> Test
-- | The status of a testcase.
data TestStatus = Passed | Failed | NotImplemented | Error

-- | The result of running a test.
data TestResult = UnitTR   { test_expression      :: String
                           , test_result_expected :: String
                           , test_result_actual   :: String
                           , test_status          :: TestStatus }
                | RandomTR { test_property        :: String
                           , test_status          :: TestStatus
                           , test_counter_example :: String     }

-- * Creating new tests
-- ----------------------------------------------------------------------------

-- | Create a unit test.
{- |
The function takes three arguments:

  * The first is a string representation of the expression to be evaluated,
  * the second is the actual expression, and
  * the third is the expected result.

Since we want to be able to print the result and also compare the actual result
of the expression with the expected one, the type under consideration should be
an instance of 'Show' and 'Eq'.

As an example, we could create a test for the associativity of list
concatenation as follows:

> test1 :: Test
> test1 = unitTest "[1,2] ++ ([3,4] ++ [5,6]) == ([1,2] ++ [3,4]) ++ [5,6]" -- Expression as a String
>                  ([1,2] ++ ([3,4] ++ [5,6]) == ([1,2] ++ [3,4]) ++ [5,6]) -- Expression to evaluate
>                  True                                                     -- Expected result
-}
unitTest :: (Eq a, Show a) => String -> a -> a -> Test
unitTest = UnitTest

-- | Create a random test to be checked using QuickCheck.
{- |
The function takes three arguments:

  * The first is a string represenation of the property to check,
  * the second is the number of the arguments the property quantifies over, and
  * the third is the property to check.

> test2 :: Test
> test2 = randomTest "forall m n k. ((n ++ m) ++ k) == (n ++ (m ++ k))" -- Property as a String
>                    3                                                  -- Number of arguments
>                    (\n m k -> ((n ++ m) ++ k) == (n ++ (m ++ k)))     -- Property to check
-}
randomTest :: Testable prop => String -> Int -> prop -> Test
randomTest = RandomTest


-- TODO write specification

unitTestIO :: String -> IO a -> [String] -> [String] -> Test
unitTestIO = UnitTestIO

-- * Running Tests
-- ----------------------------------------------------------------------------

-- | Run many tests.
runTests :: [Test] -> IO [TestResult]
runTests = mapM runTest

-- | Run a single test (either unit test or random test).
runTest :: Test -> IO TestResult
runTest (UnitTest   s e v) = runUnitTest   s e v
runTest (RandomTest s i p) = runRandomTest s i p
runTest (UnitTestIO f s i p) =  runUnitTestIO f s i p

-- | Create a new test. The arguments represent:
--   * rep: A string representation of the expression to be evaluated
--   * expression: The actual expression to evaluate
--   * expected: The expected resulting value
runUnitTest :: (Eq a, Show a) => String -> a -> a -> IO TestResult
runUnitTest rep expression expected = do
  mb_res <- evaluateTest expression expected
  return $ case mb_res of
    Right res
      | res == expected     -> def { test_result_actual = show res
                               , test_status        = Passed }
      | otherwise           -> def { test_result_actual = show res
                               , test_status        = Failed }
                               
    Left s -> case fromException s :: Maybe ErrorCall of
      Just e -> def { test_result_actual = removeTrailingWhiteSpace $ show e
                      , test_status        = NotImplemented }
      Nothing -> case fromException s :: Maybe SomeException of
        Just e -> def { test_result_actual = removeTrailingWhiteSpace $ show e
                        , test_status        = Error }
        Nothing -> def { test_status          = Passed
                        , test_counter_example = "insufficient-testing" }
                           
    -- Left (ErrorCall e)      -> def { test_result_actual = removeTrailingWhiteSpace $ show e
    --                            , test_status        = NotImplemented }
    -- Left (SomeException e)  -> def { test_result_actual = removeTrailingWhiteSpace $ show e
    --                            , test_status        = Failed }
  where
    def = UnitTR { test_expression      = rep
                 , test_result_expected = show expected
                 , test_result_actual   = undefined
                 , test_status          = undefined }

-- | Evaluate an expression. Return (Left str) if it crashes or (Right v) where
-- v is the result.
evaluateTest :: (Show a, Eq a,Exception e) => a -> a -> IO (Either e a)
evaluateTest a expected =
  (let x = a
   in  x               `seq`
       (x == expected) `seq`
       show x          `deepSeqFixerUpper`
       return (Right x)
  ) `catch`
      \(e) -> return (Left  e)

  where
    deepSeqFixerUpper :: String -> x -> x
    deepSeqFixerUpper []     e = e
    deepSeqFixerUpper (c:cs) e = c `seq` deepSeqFixerUpper cs e



-- | Parse the result of QuickCheck to isolate the arguments of the counter
-- example which made the test fail.
counterExample :: Int -> String -> String
counterExample no_args s
  | length ls >= no_args+1 = intercalate "," (take no_args (tail ls))
  | otherwise              = s
  where ls = lines s

-- | Run a random test.
runRandomTest :: Testable prop => String -> Int -> prop -> IO TestResult
runRandomTest desc no_args prop = do
  qres <- quickCheckWithResult (stdArgs { chatty = False {-, maxDiscardRatio = 100 -} }) prop
  return $ case qres of
    Success {} -> def { test_status          = Passed
                      , test_counter_example = "" }
    Failure {}
      | Just s <- theException qres ->
          case fromException s :: Maybe ErrorCall of
            Just e ->
              def { test_status          = NotImplemented
                  , test_counter_example = removeTrailingWhiteSpace (show e) } -- the exception
            Nothing ->
              case fromException s :: Maybe SomeException of
                Just e -> 
                  def { test_status           = Error
                        ,test_counter_example = removeTrailingWhiteSpace (show e) } -- the exception
                Nothing -> def { test_status           = Passed
                                , test_counter_example = "insufficient-testing" }
      | otherwise {- Nothing-} ->
          def { test_status              = Error
              , test_counter_example     = counterExample no_args (output qres) }
    _other -> -- TODO: We should find a way to do better
              -- error ("Internal error, contact the maintainer\n" ++ show other)
              def { test_status          = Passed
                  , test_counter_example = "insufficient-testing" }
    where
      def = RandomTR { test_property        = desc
                     , test_status          = undefined
                     , test_counter_example = undefined }

runUnitTestIO :: String -> IO a -> [String] -> [String] -> IO TestResult
runUnitTestIO rep expression i expected = do
  mb_res <- evaluateIOTest (fakeIO expression (unlines i)) 
  case mb_res of
    Right resInt -> do
      if unlines expected == resInt then
        return def {test_result_actual = resInt,
                    test_status = Passed}
      else
        return def {test_result_actual = resInt,
                    test_status = Failed}
    Left e                     -> return def { test_result_actual = removeTrailingWhiteSpace e
                                        , test_status        = NotImplemented }
  where
    def = UnitTR { test_expression      = rep
                , test_result_expected = unlines expected
                , test_result_actual   = undefined
                , test_status          = undefined }
                     



-- * Printing the Results
-- ----------------------------------------------------------------------------

-- | Run a list of tests and print the results to the stdout in JSON format.
runAndPrintTestsJSON :: [Test] -> IO ()
runAndPrintTestsJSON tests = runTests tests >>= printTestResultsJSON

instance Show TestStatus where
  show Passed         = show "passed"
  show Failed         = show "failed"
  show NotImplemented = show "not-implemented"
  show Error          = show "error"

-- | Print all TestResults in JSON format.
printTestResultsJSON :: [TestResult] -> IO ()
printTestResultsJSON results = putStrLn $ render $ braces doc
  where
    jresults = map pprTestResultJSON results
    doc      = hang (dQuoteText "test-results" <+> colon)
                    4 (brackets (vcat $ punctuate comma jresults))

-- | Print a single TestResult in JSON format.
pprTestResultJSON :: TestResult -> Doc
pprTestResultJSON result = braces $ vcat $ punctuate comma
    [ jsonTag "test-type" index
    , jsonTag "test-result" (braces $ vcat $ punctuate comma assoc) ]
  where
    index
      | isUnitTR result = dQuoteText "unit-test"
      | otherwise       = dQuoteText "random-test"

    assoc = case result of
      UnitTR expr expected actual status ->
        [ jsonTag "expression"      (text (show expr    ))
        , jsonTag "expected-result" (text (show expected))
        , jsonTag "actual-result"   (text (show actual  ))
        , jsonTag "status"          (text (show status  )) ]

      RandomTR prop status counter ->
        [ jsonTag "property"        (text (show prop    ))
        , jsonTag "status"          (text (show status  ))
        , jsonTag "counter-example" (text (show counter )) ]

-- | Tag a JSON value with a tag.
jsonTag :: String -> Doc -> Doc
jsonTag tag value = dQuoteText tag <+> colon <+> value

-- | Check whether a test result comes from a unit test.
isUnitTR :: TestResult -> Bool
isUnitTR (UnitTR   {}) = True
isUnitTR (RandomTR {}) = False

-- | Turn a string into a doc and add double quotes around it. Useful for JSON
-- fields and tags.
dQuoteText :: String -> Doc
dQuoteText = doubleQuotes . text

-- | Remove trailing whitespace.
removeTrailingWhiteSpace :: String -> String
removeTrailingWhiteSpace = reverse . dropWhile isSpace . reverse

-- ============================================================================

-- | Run a list of tests and print the results in a terminal. The boolean
-- passed determines whether colors shall be used.
runAndPrintTestsLocally :: Bool -> [Test] -> IO ()
runAndPrintTestsLocally col tests = runTests tests >>= printTestResultsLocally col

-- | Print all TestResults in a terminal. The boolean passed determines whether
-- colors shall be used.
printTestResultsLocally :: Bool -> [TestResult] -> IO ()
printTestResultsLocally col results = zipWithM_ ppr_one [1..length jresults] jresults
  where
    jresults = map (pprTestResultLocally col) results
    ppr_one i (ty, hd, bd) = putStrLn . (++"\n") . render
                           $ hang (text "Test" <+> int i <+> ty <+> colon <+> hd) 2 bd

-- | Command-line color identifiers.
red, green, yellow :: Int
(red, green, yellow) = (31, 32, 33)

-- | Add color (command-line) to a string.
colorise :: Int -> String -> String
colorise colorCode s = "\ESC[0;" ++ show colorCode ++ "m" ++ s ++ "\ESC[0m"

-- | Pretty print a test result locally. The boolean represents whether there
-- should be colors used.
pprTestResultLocally :: Bool -> TestResult -> (Doc, Doc, Doc) -- type, header and rest
pprTestResultLocally col result
  = ( test_type
    , test_header
    , vcat (map (\(x,y) -> x <+> colon <+> y) assoc)
    )
  where
    test_header = case result of
      UnitTR   {} -> text (test_expression result)
      RandomTR {} -> text (test_property   result)

    test_type = parens $ text $ case result of
      UnitTR   {} -> "unit test"
      RandomTR {} -> "random test"

    assoc = case result of
      UnitTR _expr expected actual status ->
        [ (text "expected result", text (show expected))
        , (text "actual result"  , text (show actual)  )
        , (text "status"         , configShow status   ) ]

      RandomTR _prop status counter ->
        [ (text "status"         , configShow status)
        , (text "counter-example", text counter     ) ]

    configShow :: TestStatus -> Doc
    configShow = let f = if col then
                            case test_status result of
                             Passed         -> colorise green
                             Failed         -> colorise red
                             NotImplemented -> colorise yellow
                             Error          -> colorise red
                          else
                            id
                 in text .  f . show

-- * Fake IO framework
-- ----------------------------------------------------------------------

-- | Perform an action that with access to a temporary file. The file is removed
-- after the action is completed.
withTempFile  :: FilePath                     -- ^ Path to directory for temporary file
    -> String                       -- ^ Base name for temporary file
    -> ((FilePath,Handle) -> IO a)  -- ^ Action
    -> IO a
withTempFile tmpDir base k = bracket
    (openTempFile tmpDir base)
    (\(file,h) -> hClose h >> removeFile file)
    k

-- | Perform an action with a redirected handle
withRedirect :: Handle  -- ^ Shadowing handle
    -> Handle  -- ^ Shadowed handle
    -> IO a    -- ^ Action in which the redirect takes place
    -> IO a
withRedirect new old act = bracket
    (do buffering <- hGetBuffering old
        dupH      <- hDuplicate old
        hDuplicateTo new old
        return (dupH,buffering)
    )
    (\(dupH,buffering) -> do
        hDuplicateTo dupH old
        hSetBuffering old buffering
        hClose dupH
    )
    (\_ -> act)

-- | Perform an action with explicit input\/output connected to
-- @`stdin`@\/@`stdout`@
fakeIO  :: IO a   -- ^ Action
    -> String     -- ^ Input to send to @stdin@
    -> IO String  -- ^ Result from @stdout@
fakeIO act inp = do
    tmpDir <- getTemporaryDirectory
    withTempFile tmpDir "fakeInput" $ \(inpFile,inpH) ->
      withTempFile tmpDir "fakeOutput" $ \(outFile,outH) -> do
        withRedirect outH stdout $
          withRedirect inpH stdin $ do
            hPutStr inpH inp
            hSeek inpH AbsoluteSeek 0
            act
            hFlush stdout
            hSeek outH AbsoluteSeek 0
            str <- hGetContents outH
            str `deepseq` return str


evaluateIOTest :: (IO a) -> IO (Either String a)
evaluateIOTest a = fmap Right a `catch` \(SomeException e) -> return (Left (show e))
--  a >>= \b -> (a `seq` return (Right a)) `catch` \(SomeException e) -> return (Left (show e))




-- * Upload file to Esystant
-- ------------------------------------------------------------------
data Flag
  = Server                -- -s
  deriving (Show,Eq)

options :: [OptDescr Flag]
options =
   [
    Option ['s'] []       (NoArg Server)          "Run on server"
   ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ic [OPTION...] files..."
    
retrieveIniValue :: Either String T.Text ->  IO T.Text
retrieveIniValue val = 
  case val of 
    Left _  -> error "Config file not complete"
    Right x -> return x

postSubmission :: T.Text -> T.Text -> T.Text -> FilePath -> IO (Response ())
postSubmission uid token assignmentName filePath = do
    manager <- newManager tlsManagerSettings
    req     <- parseRequest "https://esystant.be/api/post/new_submission/local/"
    req2    <- formDataBody    [partBS  (T.pack "user_id") (TE.encodeUtf8   uid),
                              partBS (T.pack "token") (TE.encodeUtf8  token),
                              partBS (T.pack "cat_assignment") (TE.encodeUtf8   assignmentName),
                              partFileSource  (T.pack "file") (filePath ++ "/Template.hs")] req
    httpNoBody req2 manager
    

handleStatus :: Response a -> IO()
handleStatus r = 
  case statusCode $ responseStatus r of 
    200  -> putStrLn "Thank you for submiting your solution to Esystant!"
    401  -> putStrLn "Authentication failed, please check that you are using the right config file!" 
    503  -> putStrLn "Server not reachable, please try again later!"
    _    -> putStrLn "Something went wrong, please try again!" 

processSubmission :: [Test] ->  IO ()
processSubmission allTests = do
    (as,_) <- getArgs >>= compilerOpts
    if  Server `elem` as then 
      do
        runAndPrintTestsJSON allTests
    else
      do
        iniFile <- readIniFile "config_upload.ini"
        
        runAndPrintTestsLocally True allTests
        case iniFile of 
          Left str  -> putStrLn "Config file not found -> submission not send to esystant"
          Right ini -> do
            filePath <- getCurrentDirectory 
            asID     <- retrieveIniValue $ lookupValue esystantT assignmentT ini
            userID   <- retrieveIniValue $ lookupValue esystantT esidT ini
            token    <- retrieveIniValue $ lookupValue esystantT tokenT ini          
            response <- postSubmission userID token asID filePath
            putStrLn "Disclaimer by adding the config file your solution is uploaded to the Esystant server"
            handleStatus response
        where
          esystantT   = T.pack "esystant"
          assignmentT = T.pack "assignment"
          esidT       = T.pack "ES-id"
          tokenT      = T.pack "token"


