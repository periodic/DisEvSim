{-# LANGUAGE TemplateHaskell #-}
module Main where

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

prop_EventsInOrder :: [(Time, TestEvent)] -> Bool
prop_EventsInOrder events =
    let fullQueue = foldr (uncurry enqueue) emptyQueue ((fmap wrap) <$> events)
        dequeued = unfoldr (dequeue) fullQueue
     in inOrder . map fst $ dequeued
    where
        inOrder (a:b:cs) = if a < b then inOrder (b:cs) else False
        inOrder _ = True

main :: IO ()
main = runQuickCheck $quickCheckAll
