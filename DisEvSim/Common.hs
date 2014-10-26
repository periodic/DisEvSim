{-# LANGUAGE TemplateHaskell #-}
module DisEvSim.Common ( module DisEvSim.Common
                       , def
                       ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import Data.Map.Strict (Map, empty)
import Data.Sequence (Seq)
import Data.Typeable

-- | An alias for the internal time representation.
type Time = Double
-- | An alias for the internal representation of time deltas.
type TimeDelta = Double

class (Eq a, Typeable a) => EventData a where
    wrap :: a -> Event
    wrap ev = Event ev
    unwrap :: Event -> Maybe a
    unwrap (Event ev) = cast ev
    eventType :: a -> TypeRep
    eventType = typeOf

-- | A wrapper to genericize events.
data Event where
    Event :: (Typeable ev, EventData ev) => ev -> Event

instance Eq Event where
    (Event e1) == (Event e2) = 
        case cast e1 of
            Nothing -> False
            (Just e1') -> e1' == e2

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
data Config = Config deriving (Show)

instance Default Config where
    def = Config

defaultConfig :: Config
defaultConfig = def

-- | A priority queue for Events.
newtype EventQueue = EventQueue {
        queueAsMap :: Map Time (Seq Event)
    } deriving (Show)

instance Default EventQueue where
    def = EventQueue (empty)

defaultEventQueue :: EventQueue
defaultEventQueue = def

-- | A log of events and the times they occur.
type EventLog = [(Time, Event)]

-- | The handler holder
data HandlerMap world = HandlerMap {
    nextHandlerId       :: Integer,
    idToTypeMap         :: Map HandlerId TypeRep,
    typeToHandlerMap    :: Map TypeRep (Map HandlerId (HandlerWrapper world))
    }

instance Default (HandlerMap world) where
    def = HandlerMap 0 empty empty

defaultHandlerMap :: HandlerMap world
defaultHandlerMap = def

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
                          def
                          def
                          def
                          w



