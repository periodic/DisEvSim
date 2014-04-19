{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Maybe (mapMaybe)
import Data.List (sortBy)
import Test.QuickCheck
import Data.Typeable

import DisEvSim.TestUtil
import DisEvSim.Common
import DisEvSim.EventQueue
import DisEvSim.HandlerMap
import DisEvSim.Sim

main :: IO ()
main = runQuickCheck $quickCheckAll

setupState :: world -> EventLog -> SimState world
setupState w events =
    let fullQueue = foldr (uncurry enqueue) emptyQueue events
    in set evQueue fullQueue $ defaultState w

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

prop_eventsInOrder :: [(Time, Event)] -> Bool
prop_eventsInOrder events =
    let sortedEvents = sortEventList events
        st = setupState 0 sortedEvents :: SimState Int
        st' = execSim defaultConfig st simLoop
    in getLog st' == sortedEvents

prop_handlerExecutedForAllEvents :: [(Time, Event)] -> Bool
prop_handlerExecutedForAllEvents events =
    let handler (TestEvent i) = modifyWorld (+ i)
        (_, handlers') = insert undefined handler $ emptyHandlers
        st = set handlers handlers' $ setupState 0 events
        st' = execSim defaultConfig st simLoop
        expectedTotal = foldr (\(TestEvent i) -> (+ i)) 0 
                      . mapMaybe (unwrap . snd) $ events
    in view world st' == expectedTotal

-- TODO: Props about handler registration/unregistration.
prop_registeredHandlersCalled :: [(Time, Event)] -> Bool
prop_registeredHandlersCalled events =
    let st = setupState (0,0) events :: SimState (Int, Int)
        st' = execSim defaultConfig st sim
    in view world st' == sumEvents events
    where
        sim = do
            _ <- registerHandler undefined handler1
            _ <- registerHandler undefined handler2
            simLoop
        sumEvents [] = (0,0)
        sumEvents ((_, Event ev):rest) =
            let ev1 = cast ev :: Maybe TestEvent
                ev2 = cast ev :: Maybe TestEvent2
            in case ev1 of
                (Just (TestEvent i)) -> over _1 (+i) $ sumEvents rest
                Nothing -> case ev2 of
                    (Just (TestEvent2 i)) -> over _2 (+i) $ sumEvents rest
                    Nothing -> sumEvents events

prop_deregisteredHandlersNeverCalled :: [(Time, Event)] -> Bool
prop_deregisteredHandlersNeverCalled events =
    let st = setupState (0,0) events :: SimState (Int, Int)
        st' = execSim defaultConfig st sim
    in view world st' == (0,0)
    where
        sim = do
            hId1 <- registerHandler undefined handler1
            hId2 <- registerHandler undefined handler2
            deregisterHandler hId1
            deregisterHandler hId2
            simLoop
