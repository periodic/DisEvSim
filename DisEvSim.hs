{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module DisEvSim ( simulate
                , getW
                , putW
                , modW
                , after
                , Time
                ) where

import Common
import EventQueue

{-
import Language.Haskell.TH
import Data.Record.Label
-}

import Data.Functor ((<$>))
import Control.Monad.State as S

simulate :: world -> [ev -> Sim world ev ()] -> ev -> Time -> (Time, world)
simulate world handlers event maxT = evalState (runSim $ simLoop maxT) (SimState 0 (enqueue 0 event emptyQueue) handlers world)

data SimState world ev =
    SimState { stCurrTime :: ! Time
             , stEvQueue  :: EventQueue ev
             , stHandlers :: [ev -> Sim world ev ()]
             , stWorld    :: ! world
             }

newtype Sim world ev a = Sim {
    runSim :: S.State (SimState world ev) a
    } deriving (Monad, MonadState (SimState world ev), Functor)

simLoop :: Time -> Sim world ev (Time, world)
simLoop maxT =
    do  mEv <- nextEvent
        case mEv of
            Nothing ->  -- Terminate if no more events are left in the queue.
                do  t   <- stCurrTime <$> get
                    w   <- stWorld    <$> get
                    return (t,w)
            Just ev ->
                do  st <- get
                    let t   = stCurrTime st
                        w   = stWorld    st
                        hs  = stHandlers st
                    if (t >= maxT)
                        then return (maxT, w) -- terminate as well.
                        else let actions = map (\h -> h ev) hs
                                 world'  = foldl (>>) (return ()) actions
                              in world' `seq` world' >> simLoop maxT

nextEvent :: Sim world ev (Maybe ev)
nextEvent =
    do  st@(SimState _ q _ _) <- S.get
        case dequeue q of
            (Nothing, _)        -> return Nothing
            (Just (t', ev), q') -> do
                put $ st { stEvQueue = q', stCurrTime = t' }
                return . Just $ ev


getW :: Sim world ev world
getW = stWorld <$> get

putW :: world -> Sim world ev ()
putW w' = modify $ \st -> st { stWorld = w' }

modW :: (world -> world) -> Sim world ev ()
modW f = modify $ \st -> st { stWorld = f (stWorld st) }

after :: Time -> ev -> Sim world ev ()
after dt e =
    do  st <- get
        let t = stCurrTime st
            q = stEvQueue  st
        put $ st { stEvQueue = enqueue (t + dt) e q }

