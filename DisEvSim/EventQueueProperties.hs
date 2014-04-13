module Main where

import System.Exit
import Test.QuickCheck

prop1 :: Char -> Bool
prop1 c = c == 'a'

main = do
    result <- quickCheckResult prop1
    case result of
        Success _ _ _ -> exitSuccess
        GaveUp _ _ _ -> exitFailure
        Failure _ _ _ _ _ _ _ _ _ _ -> exitFailure
        NoExpectedFailure _ _ _ -> exitFailure
