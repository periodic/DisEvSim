{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Typeable
import Test.QuickCheck.All

import DisEvSim.TestUtil
import DisEvSim.Common
import DisEvSim.Sim

main :: IO ()
main = runQuickCheck $quickCheckAll

addEvents :: [(Time, Event)] -> Sim world ()
addEvents = mapM_ (uncurry addEvent)
  where
    addEvent :: Time -> Event -> Sim world ()
    addEvent t ev = do
            let ev1 = unwrap ev :: Maybe TestEvent
                ev2 = unwrap ev :: Maybe TestEvent2
            maybe (return ()) (after t) ev1
            maybe (return ()) (after t) ev2

filterByType :: Typeable ev => [Event] -> [ev]
filterByType = mapMaybe (\(Event event) -> cast event)

sortEventList :: [(Time, Event)] -> [(Time, Event)]
sortEventList events =
    let compareEntry e1 e2 = fst e1 `compare` fst e2
    in sortBy compareEntry events

handler1 :: TestEvent -> Sim (Int, Int) ()
handler1 (TestEvent i) = modifyWorld $ over _1 (+i)

handler2 :: TestEvent2 -> Sim (Int, Int) ()
handler2 (TestEvent2 i) = modifyWorld $ over _2 (+i)

-- | Tests that events are triggered in order.
prop_eventsInOrder :: [(Time, TestEvent)] -> Bool
prop_eventsInOrder events =
    let sortedEvents = sortEventList $ over (mapped . _2) wrap events
        initialize = mapM_ (\(t, ev) -> after t ev) events
        (_, log', _) = simulate defaultConfig (0 :: Integer) initialize
    in log' == sortedEvents

-- | Tests that handlers can be registered and will be triggered.
prop_registeredHandlersCalled :: [(Time, Event)] -> Bool
prop_registeredHandlersCalled events =
    let (_, _, w') = simulate defaultConfig (0, 0) sim
    in w' == sumEvents events
    where
        sim = do
            _ <- registerHandler undefined handler1
            _ <- registerHandler undefined handler2
            addEvents events
        sumEvents [] = (0,0)
        sumEvents ((_, Event ev):rest) =
            let ev1 = cast ev :: Maybe TestEvent
                ev2 = cast ev :: Maybe TestEvent2
            in case ev1 of
                (Just (TestEvent i)) -> over _1 (+i) $ sumEvents rest
                Nothing -> case ev2 of
                    (Just (TestEvent2 i)) -> over _2 (+i) $ sumEvents rest
                    Nothing -> sumEvents events

-- | Tests that deregistered handlers are no longer called.
prop_deregisteredHandlersNeverCalled :: [(Time, Event)] -> Bool
prop_deregisteredHandlersNeverCalled events =
    let (_, _, w') = simulate defaultConfig (0,0) initialize
    in w' == (0,0)
    where
        initialize = do
            hId1 <- registerHandler undefined handler1
            hId2 <- registerHandler undefined handler2
            deregisterHandler hId1
            deregisterHandler hId2
            addEvents events

-- | Triggers the next event in the list after a delay given by the current
--   event.
eventListHandler :: [TestEvent] -> TestEvent -> Sim Int ()
eventListHandler [] (TestEvent i) = modifyWorld (+i)
eventListHandler (ev:evs) (TestEvent i) = do
  _ <- once undefined $ eventListHandler evs
  modifyWorld (+i)
  after (fromIntegral i) ev

prop_AllTriggeredEventsAreCalled :: [TestEvent] -> Bool
prop_AllTriggeredEventsAreCalled [] = True
prop_AllTriggeredEventsAreCalled events@(ev:evs) =
    let (_, _, w') = simulate defaultConfig 0 sim
        expectedTotal = sum . map (\(TestEvent i) -> i) $ events
    in w' == expectedTotal
    where
        sim = once undefined (eventListHandler evs) >> after 0 ev

prop_SimulateStopsInAppropriateTime :: [TestEvent] -> Bool
prop_SimulateStopsInAppropriateTime events =
  let recordLastHandler (TestEvent i) = setWorld i
      maxT = 100
      initialize = do
          _ <- registerHandler undefined recordLastHandler
          mapM_ (\ev@(TestEvent i) -> after (fromIntegral i) ev) events
      config = defaultConfig { maxTime = Just maxT }
      (t, _, i') = simulate config 0 initialize
  in t <= maxT && fromIntegral i' <= maxT
