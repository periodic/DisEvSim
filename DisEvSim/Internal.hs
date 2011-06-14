module DisEvSim.Internal  where

import DisEvSim.Common
import DisEvSim.EventQueue
import DisEvSim.Handler

import Data.Functor ((<$>))
import qualified Data.DList as L
import Control.Monad.State as S

simulate :: world -> [(String, ev -> Sim world ev ())] -> ev -> Time -> (Time, [(Time, ev)], world)
simulate world handlers event maxT = evalState (runSim $ simLoop maxT) initialState
    where
        initialState = SimState { stCurrTime    = 0
                                , stEvQueue     = (enqueue 0 event emptyQueue)
                                , stEvLog       = L.empty
                                , stHandlers    = handlersFromList handlers
                                , stWorld       = world
                                }

simLoop :: Time -> Sim world ev (Time, [(Time,ev)], world)
simLoop maxT =
    do  mEv <- nextEvent
        case mEv of
            Nothing ->  -- Terminate if no more events are left in the queue.
                do  st <- get
                    let t   = stCurrTime st
                        w   = stWorld    st
                        log = {-# SCC "makeLog1" #-} L.toList . stEvLog $ st
                    return (t,log,w)
            Just (t, ev) ->
                do  st <- get
                    let w   = stWorld    st
                        hs  = stHandlers st
                        log = {-# SCC "makeLog2" #-} L.toList . stEvLog $ st
                    if (t > maxT)
                        then return (maxT, log, w) -- terminate as well.
                        else let world'  = processHandlers ev hs
                              in world' `seq` do
                                    put $ st { stCurrTime = t }
                                    appendLog t ev
                                    world'
                                    simLoop maxT

nextEvent :: Sim world ev (Maybe (Time, ev))
nextEvent =
    do  st <- S.get
        let q = stEvQueue st
        case dequeue q of
            (Nothing, _)        -> return Nothing
            (Just event, q') -> do
                put $ st { stEvQueue = q'}
                return . Just $ event

appendLog :: Time -> ev -> Sim world ev ()
appendLog t e = do
    st <- get
    let log' = L.snoc (stEvLog st) (t, e)
    put $ st { stEvLog = log' }

-- Public functions
getW :: Sim world ev world
getW = stWorld <$> get

putW :: world -> Sim world ev ()
putW w' = modify $ \st -> st { stWorld = w' }

modW :: (world -> world) -> Sim world ev ()
modW f = modify $ \st -> st { stWorld = f (stWorld st) }

getT :: Sim world ev Time
getT = stCurrTime <$> get

after :: Time -> ev -> Sim world ev ()
after dt e =
    do  st <- get
        let t = stCurrTime st
            q = stEvQueue  st
        put $ st { stEvQueue = enqueue (t + dt) e q }

addHandler :: String -> Handler world ev -> Sim world ev ()
addHandler name h = do
    st <- get
    let hs = stHandlers st
        hs' = insertHandler name h hs
    put $ st { stHandlers = hs' }
