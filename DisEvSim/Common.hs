{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, GADTs #-}
module DisEvSim.Common where

import Control.Monad.State
import Data.DList (DList)
import Data.Map (Map)
import Data.Typeable

-- | An alias for the internal time representation.
type Time = Double
-- | An alias for the internal representation of time deltas.
type TimeDelta = Double

data EventList where
    EventListCons :: Event a => a -> EventList -> EventList
    EventListNil :: EventList

instance Show EventList where
    show EventListNil = "[]"
    show (EventListCons a rest) = show (typeOf a) ++ ":" ++ show rest

class Typeable a => Event a where
    isEvent :: a -> Bool
    isEvent _ = True

class Handler a where
    handles :: a -> EventList
    handle :: Event e => a -> e -> Sim world ()

-- | Configuration options.
data Config = Config { enableLog :: Bool
                     } deriving (Show)

{-
-- | A log of events and the times they occur.
type EventLog ev = DList (Time, ev)

type HandlerMap world = Event ev => Map HandlerId (Handler world ev)

data HandlerId = HandlerId String
                 deriving (Show, Eq, Ord)

data EventQueue = EventQueue
                  deriving (Show)


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

