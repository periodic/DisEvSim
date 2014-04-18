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

data TestEvent2 = TestEvent2 Int
                 deriving (Show, Eq, Ord, Typeable)

instance EventData TestEvent2 where

instance Arbitrary TestEvent2 where
    arbitrary = TestEvent2 <$> arbitrary

data TestHandler = TestHandler (TestEvent -> Sim Int Int)

emptyHandler :: TestEvent -> Sim Int ()
emptyHandler = return . const ()

instance Arbitrary Event where
    arbitrary = oneof $ [wrap <$> (arbitrary :: Gen TestEvent),
                         wrap <$> (arbitrary :: Gen TestEvent2)]

runQuickCheck :: IO Bool -> IO ()
runQuickCheck tests = do
    result <- tests
    if result
        then exitSuccess
        else exitFailure
