{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, GADTs #-}
module DisEvSim.Common where

import Control.Monad.State
import Data.DList (DList)
import Data.Map (Map)
import Data.Typeable

data EventList where
    EventListCons :: Event a => EventList -> a -> EventList
    EventListNil :: EventList
    deriving (Show)

class (Typeable a, Show a) => Event a where
    isEvent :: a -> Bool
    isEvent _ = True

class Handler a where
    handles :: a -> EventList
    handle :: Event e => a -> e -> Sim ()

{-
import Data.Record.Label

import qualified Control.Category as Cat

get = getL
set = setL

a <.> b = (Cat..) a b 
-}

-- | Configuration options.
data Config = Config { enableLog :: Bool
                     } deriving (Show)
-- | An alias for the internal time representation.
type Time = Double
-- | An alias for the internal representation of time deltas.
type DTime = Double

-- | A log of events and the times they occur.
type EventLog ev = DList (Time, ev)

-- | The type of an event handler.  It takes events, and performs some action in the world.
type Handler world ev = Event ev => ev -> Sim world ev ()

type HandlerMap world ev = Event ev => Map HandlerId (Handler world ev)

data HandlerId = HandlerId String
                 deriving (Show, Eq, Ord)

newtype EventQueue a = EventQueue (Map Time [a])
    deriving (Show)


data SimState world ev = Event ev =>
    SimState { stCurrTime :: ! Time
             , stEvQueue  :: EventQueue ev
             , stEvLog    :: EventLog ev
             , stHandlers :: HandlerMap world ev
             , stWorld    :: ! world
             , stConfig   :: Config -- TODO: Change this to a reader
             }

-- | The sim manad.
newtype Sim world ev a = Event ev => Sim {
    runSim :: State (SimState world ev) a
    } deriving (Monad, MonadState (SimState world ev), Functor)

