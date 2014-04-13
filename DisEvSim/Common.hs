module DisEvSim.Common where

import Control.Monad.State
import Data.Map.Strict (Map)
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

data Event where
    Event :: EventData a => a -> Event

instance Show Event where
    show (Event ev) = "Event " ++ (show $ typeOf ev) ++ ")"

class Handler a where
    handles :: a -> [Event]
    handle :: EventData e => a -> e -> Sim world ()

-- | Configuration options.
data Config = Config { enableLog :: Bool
                     } deriving (Show)

-- | A priority queue for Events.
newtype EventQueue = EventQueue {
        queueAsMap :: Map Time (Seq Event)
    } deriving (Show)

{-
-- | A log of events and the times they occur.
type EventLog ev = DList (Time, ev)

type HandlerMap world = Event ev => Map HandlerId (Handler world ev)

data HandlerId = HandlerId String
                 deriving (Show, Eq, Ord)

data SimState world = Event =>
    SimState { stCurrTime :: ! Time
             , stEvQueue  :: EventQueue
             , stEvLog    :: EventLog
             , stHandlers :: HandlerMap world
             , stWorld    :: ! world
             , stConfig   :: Config -- TODO: Change this to a reader
             }
-}
data SimState world =
    SimState { stCurrTime :: ! Time
             , stWorld    :: ! world
             , stConfig   :: Config -- TODO: Change this to a reader
             }

-- | The sim manad.
newtype Sim world a = Sim {
    runSim :: State (SimState world) a
    } deriving (Monad, MonadState (SimState world), Functor)

