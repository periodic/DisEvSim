{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative ((<$>))
import Control.Lens
import Data.List (unfoldr)
import Data.Maybe (mapMaybe)
import Test.QuickCheck.All
import Test.QuickCheck.Modifiers

import DisEvSim.Common
import DisEvSim.EventQueue
import DisEvSim.TestUtil

-- | Tests that events are pulled in the order they are inserted.
prop_PreserveOrder :: [TestEvent] -> Bool
prop_PreserveOrder events =
    let fullQueue = foldr (enqueue 1 ) defaultEventQueue (map wrap events)
        dequeued = unfoldr (dequeue) fullQueue
     in mapMaybe (unwrap . snd) dequeued == events

-- | When events are inserted with a timestamp they are pulled in order of timestamp.
prop_EventsInOrder :: OrderedList (Time, TestEvent) -> Bool
prop_EventsInOrder (Ordered events) =
    let fullQueue = foldr (uncurry enqueue) defaultEventQueue ((fmap wrap) <$> events)
        dequeued = unfoldr (dequeue) fullQueue
        unwrappedDequeued = mapMaybe (\(t, mEv) -> ((,) t) <$> mEv) . over (mapped._2) unwrap $ dequeued 
     in events == unwrappedDequeued

main :: IO ()
main = runQuickCheck $quickCheckAll
