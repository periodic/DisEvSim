module DisEvSim.EventQueue ( emptyQueue
                           , enqueue
                           , dequeue
                           , EventQueue
                           ) where

import qualified Data.Map.Strict as M
import Data.Sequence ((><), ViewL((:<)))
import qualified Data.Sequence as S

import DisEvSim.Common

enqueue :: Time -> Event -> EventQueue -> EventQueue
enqueue t ev (EventQueue queue) =
    EventQueue $ M.insertWith (><) t (S.singleton ev) queue

dequeue :: EventQueue -> Maybe ((Time, Event), EventQueue)
dequeue (EventQueue queue) = 
    if M.null queue
    then Nothing
    else
        let ((t, s), queue') = M.deleteFindMin queue
            v = S.viewl s
        in case v of
            S.EmptyL -> error "Empty bucket in EventQueue"
            (ev :< s') ->
                if S.null s'
                then Just ((t, ev), EventQueue queue')
                else 
                    let queue'' = M.insert t s' queue'
                    in Just ((t, ev), EventQueue queue'')
