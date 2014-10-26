-- | A simple discrete event simulator.
module DisEvSim.Sim where

import Control.Lens
--import Control.Monad.State as S
import Data.Typeable

import DisEvSim.Common
import DisEvSim.EventQueue
import DisEvSim.HandlerMap

-- | Gets the log.
getLog :: SimState world -> EventLog
getLog st = reverse . view evLog $ st

-- | Gets th enext event from the queue.
getNextEvent :: Sim world (Maybe (Time, Event))
getNextEvent = do
    queue <- use evQueue
    let mEventAndQueue = dequeue queue
    case mEventAndQueue of
        Nothing -> return Nothing
        Just ((t, ev), queue') -> do
            assign evQueue queue'
            return $ Just (t, ev)

-- | Adds an event to the log
logEvent :: Time -> Event -> Sim world ()
logEvent t event = evLog %= ((t, event) :)

-- | The main simulator loop.
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

-- | Runs all handlers for an event.
processEvent :: (Typeable world) => Event -> Sim world ()
processEvent (Event event) = do
    eventHandlers <- uses handlers (getAllForEvent event)
    mapM_ ($ event) eventHandlers

-- | Gets the state
getWorld :: Sim world world
getWorld = use world

-- | Sets the state
setWorld :: world -> Sim world ()
setWorld = assign world

-- | Updates the state.
modifyWorld :: (world -> world) -> Sim world ()
modifyWorld f = world %= f

-- | Adds a handler that will respond to all future matching events.
registerHandler :: EventData ev => ev -> Handler world ev -> Sim world HandlerId
registerHandler ev h = do
    hMap <- use handlers
    let (hId, hMap') = insert ev h hMap
    assign handlers hMap'
    return hId

-- | Runs a handler only once.  Deregistering it after it is run.
once :: EventData ev => ev -> Handler world ev -> Sim world HandlerId
once ev h = do
    hMap <- use handlers
    let hId = peekNextId hMap
    registerHandler ev $ wrapHandler hId h
  where
    wrapHandler hId h' ev' = do
      deregisterHandler hId
      h' ev'

-- | Removes a handler if present.
deregisterHandler :: HandlerId -> Sim world ()
deregisterHandler hId = do
    hMap <- use handlers
    let hMap' = delete hId hMap
    assign handlers hMap'

-- | Returns the current time.
getCurrTime :: Sim world Time
getCurrTime = use currTime

-- | Adds an new event to the queue n seconds from now.
after :: EventData ev => Time -> ev -> Sim world ()
after dt ev = do
  t <- use currTime
  evQueue %= enqueue (t + dt) (wrap ev)

-- | Brings it all together by running the whole simulation.
simulate :: (EventData ev, Typeable world) => Config -> world -> Sim world () -> ev -> Time -> (Time, EventLog, world)
simulate _ w sim ev _ =
  let eventQueue = enqueue 0 (wrap ev) defaultEventQueue
      state = set evQueue eventQueue $ defaultState w
      state' = execSim defaultConfig state (sim >> simLoop)

      t' = state' ^. currTime
      log' = getLog state'
      w' = state' ^. world
  in (t', log', w')


