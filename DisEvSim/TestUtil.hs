module DisEvSim.TestUtil where

import Data.Typeable
import Control.Applicative
import System.Exit (exitSuccess, exitFailure)
import Test.QuickCheck

import DisEvSim.Common

data TestEvent = TestEvent Int
                 deriving (Show, Eq, Ord, Typeable)

instance EventData TestEvent where

instance Arbitrary TestEvent where
    arbitrary = TestEvent <$> arbitrary

runQuickCheck :: IO Bool -> IO ()
runQuickCheck tests = do
    result <- tests
    if result
        then exitSuccess
        else exitFailure