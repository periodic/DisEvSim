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

-- | Runs a handler only once.  Deregistering it after it is run.
once :: EventData ev => ev -> Handler world ev -> Sim world HandlerId
once = undefined

deregisterHandler :: HandlerId -> Sim world ()
deregisterHandler hId = do
    hMap <- use handlers
    let hMap' = delete hId hMap
    assign handlers hMap'

-- | Returns the current time.
getCurrTime :: Sim world Time
getCurrTime = use currTime

-- | Adds an new event to the queue n seconds from now.
after :: Time -> ev -> Sim world ()
after = undefined

simulate :: Config -> world -> [(String, ev -> Sim world ())] -> ev -> Time -> (Time, [(Time, ev)], world)
simulate {- config state handlers event maxTime -} = undefined

