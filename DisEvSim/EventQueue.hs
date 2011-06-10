module DisEvSim.EventQueue ( emptyQueue
                           , enqueue
                           , dequeue
                           , EventQueue
                           ) where

import DisEvSim.Common

import Data.Map


newtype EventQueue a = EventQueue (Map Time a) 
    deriving (Show)


enqueue :: Time -> a -> EventQueue a -> EventQueue a
enqueue t ev (EventQueue map) = 
    let map' = insert t ev map
     in EventQueue map'

dequeue :: EventQueue a -> (Maybe (Time, a), EventQueue a)
dequeue q@(EventQueue map) = 
    case minViewWithKey map of
        Nothing         -> (Nothing, q)
        Just (ev, map') -> (Just ev, EventQueue map')

emptyQueue :: EventQueue a
emptyQueue = EventQueue empty
