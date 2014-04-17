{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens
import Data.Maybe (mapMaybe)
import Test.QuickCheck

import DisEvSim.TestUtil
import DisEvSim.Common
import DisEvSim.EventQueue
import DisEvSim.Sim

main :: IO ()
main = runQuickCheck $quickCheckAll

prop_eventsInOrder :: OrderedList (Time, TestEvent) -> Bool
prop_eventsInOrder (Ordered events) =
    let wrappedEvents = over (mapped._2) wrap events
        fullQueue = foldr (uncurry enqueue) emptyQueue wrappedEvents
        st = set evQueue fullQueue $ defaultState 1 :: SimState Int
        st' = execSim defaultConfig st simLoop
        unwrappedLog = mapMaybe (\(t, mEv) -> ((,) t) <$> mEv) . over (mapped._2) unwrap . getLog $ st'
    in unwrappedLog == events
