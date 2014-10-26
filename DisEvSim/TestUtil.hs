module DisEvSim.TestUtil where

import Data.Typeable
import Control.Applicative
import System.Exit (exitSuccess, exitFailure)
import Test.QuickCheck

import DisEvSim.Common

data TestEvent = TestEvent Int
                 deriving (Show, Eq, Ord, Typeable)

instance EventData TestEvent where

-- Note this creates an event with only positive integer data.
instance Arbitrary TestEvent where
    arbitrary = do
      (Positive i) <- arbitrary
      return $ TestEvent i

data TestEvent2 = TestEvent2 Int
                 deriving (Show, Eq, Ord, Typeable)

instance EventData TestEvent2 where

-- Note this instance can hold negative data.
instance Arbitrary TestEvent2 where
    arbitrary = TestEvent2 <$> arbitrary

data TestHandler = TestHandler (TestEvent -> Sim Int Int)

instance Arbitrary Event where
    arbitrary = oneof $ [wrap <$> (arbitrary :: Gen TestEvent),
                         wrap <$> (arbitrary :: Gen TestEvent2)]

emptyHandler :: TestEvent -> Sim Int ()
emptyHandler = return . const ()

runQuickCheck :: IO Bool -> IO ()
runQuickCheck tests = do
    result <- tests
    if result
        then exitSuccess
        else exitFailure
