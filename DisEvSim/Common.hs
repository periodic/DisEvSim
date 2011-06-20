{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module DisEvSim.Common where

import Data.DList (DList)
import Data.Map (Map)
import Control.Monad.State
{-
import Data.Record.Label

import qualified Control.Category as Cat

get = getL
set = setL

a <.> b = (Cat..) a b 
-}

{-----------------------------------------
 - Config
 -----------------------------------------}

data Config = Config { enableLog :: Bool
                     } deriving (Show)

{-----------------------------------------
 - Other
 -----------------------------------------}
type Time = Double
type DTime = Double

type EventLog ev = DList (Time, ev)

type Handler world ev = ev -> Sim world ev ()
type HandlerMap world ev = Map HandlerId (Handler world ev)

data HandlerId = HandlerId String
                 deriving (Show, Eq, Ord)

newtype EventQueue a = EventQueue (Map Time [a])
    deriving (Show)


data SimState world ev =
    SimState { stCurrTime :: ! Time
             , stEvQueue  :: EventQueue ev
             , stEvLog    :: EventLog ev
             , stHandlers :: HandlerMap world ev
             , stWorld    :: ! world
             , stConfig   :: Config -- TODO: Change this to a reader
             }

newtype Sim world ev a = Sim {
    runSim :: State (SimState world ev) a
    } deriving (Monad, MonadState (SimState world ev), Functor)
