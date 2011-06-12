module DisEvSim.EventQueue ( emptyQueue
                           , enqueue
                           , dequeue
                           , EventQueue
                           ) where

import DisEvSim.Common

import Data.Map


enqueue :: Time -> a -> EventQueue a -> EventQueue a
enqueue t ev (EventQueue map) =
    let map' = insertWith' (++) t [ev] map
     in EventQueue map'

dequeue :: EventQueue a -> (Maybe (Time, a), EventQueue a)
dequeue q@(EventQueue map) =
    case minViewWithKey map of
        Nothing          -> (Nothing, q)
        Just (evs, map') -> 
            case evs of
                (t, (e:[])) -> (Just (t, e), EventQueue map')
                (t, (e:es)) -> (Just (t, e), EventQueue $ insert t es map')

emptyQueue :: EventQueue a
emptyQueue = EventQueue empty
