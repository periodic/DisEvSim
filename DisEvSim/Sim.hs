-- | A simple discrete event simulator.
module DisEvSim.Sim where

import Control.Lens
--import Control.Monad.State as S
import Data.Typeable

import DisEvSim.Common
import DisEvSim.EventQueue
import DisEvSim.HandlerMap

getLog :: SimState world -> EventLog
getLog st = reverse . view evLog $ st

getNextEvent :: Sim world (Maybe (Time, Event))
getNextEvent = do
    queue <- use evQueue
    let mEventAndQueue = dequeue queue
    case mEventAndQueue of
        Nothing -> return Nothing
        Just ((t, ev), queue') -> do
            assign evQueue queue'
            return $ Just (t, ev)

logEvent :: Time -> Event -> Sim world ()
logEvent t event = evLog %= ((t, event) :)

simLoop :: Typeable world => Sim world ()
simLoop = do
    mEvent <- getNextEvent
    case mEvent of
        Nothing -> return ()
        Just (t, event) -> do
            logEvent t event
            assign currTime t
            processEvent event
            simLoop

processEvent :: (Typeable world) => Event -> Sim world ()
processEvent (Event event) = do
    eventHandlers <- uses handlers (getAllForEvent event)
    mapM_ ($ event) eventHandlers

getWorld :: Sim world world
getWorld = use world

setWorld :: world -> Sim world ()
setWorld = assign world

modifyWorld :: (world -> world) -> Sim world ()
modifyWorld f = getWorld >>= setWorld . f

registerHandler :: EventData ev => ev -> Handler world ev -> Sim world HandlerId
registerHandler ev h = do
    hMap <- use handlers
    let (hId, hMap') = insert ev h hMap
    assign handlers hMap'
    return hId

deregisterHandler :: HandlerId -> Sim world ()
deregisterHandler hId = do
    hMap <- use handlers
    let hMap' = delete hId hMap
    assign handlers hMap'

{-
import DisEvSim.Common
import DisEvSim.EventQueue
import Control.Monad.State as S

-- | Runs a simulation.
simulate :: Config -> world -> [(String, ev -> Sim world ev ())] -> ev -> Time -> (Time, [(Time, ev)], world)
simulate conf world handlers event maxT = evalSim (runSim $ simLoop maxT)
                                                  (defaultState world)

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
    if (enableLog . stConfig $ st)
        then put $ st { stEvLog = log' }
        else return ()

-- * Simulator functions

-- | Returns the state of the world.
getW :: Sim world ev world
getW = stWorld <$> get

-- | Updates the state of the world.
putW :: world -> Sim world ev ()
putW w' = modify $ \st -> st { stWorld = w' }

-- | Alters the state of the world.
modW :: (world -> world) -> Sim world ev ()
modW f = modify $ \st -> st { stWorld = f (stWorld st) }

-- | Returns the current time.
getT :: Sim world ev Time
getT = stCurrTime <$> get

-- | Adds an new event to the queue n seconds from now.
after :: Time -> ev -> Sim world ev ()
after dt e =
    do  st <- get
        let t = stCurrTime st
            q = stEvQueue  st
        put $ st { stEvQueue = enqueue (t + dt) e q }

-- | Adds a new handler for events, which will process events.
addHandler :: String -> Handler world ev -> Sim world ev ()
addHandler name h = do
    st <- get
    let hs = stHandlers st
        hs' = insertHandler name h hs
    put $ st { stHandlers = hs' }

removeHandler :: String -> Sim world ev ()
removeHandler name = do
    st <- get
    let hs = stHandlers st
        hs' = deleteHandler name hs
    put $ st { stHandlers = hs }
-}
