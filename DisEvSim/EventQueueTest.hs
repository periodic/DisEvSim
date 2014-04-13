module Main where

import System.Exit
import Test.HUnit hiding (errors, failures)

test1 :: Test
test1 = TestCase $ False @? "False"

tests :: Test
tests = test ["test1" ~: test1]

main :: IO ()
main = do
    (Counts _ _ errors failures) <- runTestTT tests
    if (errors > 0 || failures > 0)
        then exitFailure
        else exitSuccess


