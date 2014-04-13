module Main where

import System.Exit
import Test.HUnit

test1 :: Test
test1 = TestCase $ False @? "False"

tests :: Test
tests = test ["test1" ~: test1]

main = do
    counts <- runTestTT tests
    if (errors counts > 0 || failures counts > 0)
        then exitFailure
        else exitSuccess


