{-# LANGUAGE TemplateHaskell #-}
module DisEvSim.Common where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict (Map, empty)
import Data.Sequence (Seq)
import Data.Typeable

-- | An alias for the internal time representation.
type Time = Double
-- | An alias for the internal representation of time deltas.
type TimeDelta = Double

class Typeable a => EventData a where
    wrap :: a -> Event
    wrap ev = Event ev
    unwrap :: Event -> Maybe a
    unwrap (Event ev) = cast ev

-- | A wrapper to genericize events.
data Event where
    Event :: EventData a => a -> Event

instance Show Event where
    show (Event ev) = "Event " ++ (show $ typeOf ev)

-- | The sim monad.
newtype Sim world a = Sim {
    makeSim :: ReaderT Config (State (SimState world)) a
    } deriving (Monad, MonadState (SimState world), Functor, Typeable)

runSim :: Config -> SimState world -> Sim world a -> (a, SimState world)
runSim config st = flip runState st . flip runReaderT config . makeSim

evalSim :: Config -> SimState world -> Sim world a -> a
evalSim config st = fst . runSim config st

execSim :: Config -> SimState world -> Sim world a -> SimState world
execSim config st = snd . runSim config st

-- | An event handler
type Handler world e = e -> Sim world ()

-- | Wraps a handler to genericize it.
data HandlerWrapper world where
    HandlerWrapper :: EventData e => HandlerId -> Handler world e -> HandlerWrapper world

-- | Configuration options.
data Config = Config { enableLog :: Bool
                     } deriving (Show)

defaultConfig :: Config
defaultConfig = Config False

-- | A priority queue for Events.
newtype EventQueue = EventQueue {
        queueAsMap :: Map Time (Seq Event)
    } deriving (Show)

emptyQueue :: EventQueue
emptyQueue = EventQueue (empty)

-- | A log of events and the times they occur.
type EventLog = [(Time, Event)]

emptyLog :: EventLog
emptyLog = []

-- | The handler holder
data HandlerMap world = HandlerMap {
    nextHandlerId       :: Integer,
    idToTypeMap         :: Map HandlerId TypeRep,
    typeToHandlerMap    :: Map TypeRep (Map HandlerId (HandlerWrapper world))
    }

emptyHandlers :: HandlerMap world
emptyHandlers = HandlerMap 0 empty empty

newtype HandlerId = HandlerId Integer
                    deriving (Show, Eq, Ord)

data SimState world =
    SimState { _currTime        :: !Time
             , _evQueue         :: EventQueue
             , _evLog           :: EventLog
             , _handlers        :: HandlerMap world
             , _world           :: !world
             }
makeLenses ''SimState

defaultState :: world -> SimState world
defaultState w = SimState 0
                          emptyQueue
                          emptyLog
                          emptyHandlers
                          w



