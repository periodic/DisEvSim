{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This test creates a very simple simulation of a clock and a counter.
--
-- The test will use () as the event type since no data is actually required.
--
-- The test will use two handlers:
--
-- * Tick will generate a new event 1 second after each event it sees.
-- * Count will increment the simulation data by 1 for each event it sees.
--
-- The net result is that it simulates counting up 100 events separated by a
-- time delta of 1.
module Main where

import Test.HUnit hiding (errors, failures)
import System.Exit

import DisEvSim

-- Make unit be an event.
instance EventData () where

tick :: () -> Sim Integer ()
tick _ = after 1 ()

count :: () -> Sim Integer ()
count _ = modifyWorld (+1)

initialize :: Sim Integer ()
initialize = do
  _ <- registerHandler () tick
  _ <- registerHandler () count
  after 1 () -- Start it off.

runCounter :: Time -> (Time, Integer)
runCounter tMax =
    let config = defaultConfig { maxTime = Just tMax }
        (t, _, w) = simulate config 0 initialize
    in (t, w)

testCounter :: Test
testCounter = 
    let (t, w) = runCounter 100
    in TestCase $ do
        assertEqual "Elapsed time should be 100" 100 t
        assertEqual "Total count should be 100" 100 w

main :: IO ()
main = do
    (Counts _ _ errors failures) <- runTestTT $ TestList [TestLabel "counter test" testCounter]
    if (errors > 0 || failures > 0)
        then exitFailure
        else exitSuccess
