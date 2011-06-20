module DisEvSim.Debug where

import DisEvSim.Common
import DisEvSim.EventQueue
import DisEvSim.Handler
import DisEvSim.Internal

import Control.Monad.State
import Data.DList (toList, empty)

simulateDebug :: (Show world, Show ev) => Config -> world -> [(String, Handler world ev)] -> ev -> Time -> SimState world ev
simulateDebug config world handlers event maxT = execState (runSim $ simLoop maxT) initialState
    where
        initialState = SimState { stCurrTime = 0
                                , stEvQueue  = (enqueue 0 event emptyQueue)
                                , stEvLog    = empty
                                , stHandlers = handlersFromList handlers
                                , stWorld    = world
                                , stConfig   = config
                                }


instance (Show world, Show ev) => Show (SimState world ev) where
    show st =  "SimState {time: "
            ++ show (stCurrTime  st)
            ++ ", eventQueue: "
            ++ show (stEvQueue st)
            ++ ", eventLog: "
            ++ show (toList . stEvLog $ st)
            ++ ", world: "
            ++ show (stWorld st)
            ++ "}"


