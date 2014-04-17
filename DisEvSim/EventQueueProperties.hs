{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Applicative ((<$>))
import Data.List (unfoldr)
import Data.Maybe (mapMaybe)
import Test.QuickCheck

import DisEvSim.Common
import DisEvSim.EventQueue
import DisEvSim.TestUtil

prop_PreserveOrder :: [TestEvent] -> Bool
prop_PreserveOrder events =
    let fullQueue = foldr (enqueue 1 ) emptyQueue (map wrap events)
        dequeued = unfoldr (dequeue) fullQueue
     in mapMaybe (unwrap . snd) dequeued == events

prop_EventsInOrder :: OrderedList (Time, TestEvent) -> Bool
prop_EventsInOrder (Ordered events) =
    let fullQueue = foldr (uncurry enqueue) emptyQueue ((fmap wrap) <$> events)
        dequeued = unfoldr (dequeue) fullQueue
        unwrappedDequeued = mapMaybe (\(t, mEv) -> ((,) t) <$> mEv) . over (mapped._2) unwrap $ dequeued 
     in events == unwrappedDequeued

main :: IO ()
main = runQuickCheck $quickCheckAll
