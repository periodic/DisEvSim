module DisEvSim.HandlerMap where

import qualified Data.Maybe
import qualified Data.Map as M
import Data.Typeable

import DisEvSim.Common

insert :: EventData ev => ev -> Handler world ev -> HandlerMap world
                                  -> (HandlerId, HandlerMap world)
insert ev handler (HandlerMap nextId idMap typeMap) =
    let t = eventType ev
        hId = HandlerId nextId
        nextId' = nextId + 1
        idMap' = M.insert hId t idMap
        wrapper = HandlerWrapper hId handler
        singletonHandlerMap = M.singleton hId wrapper
        maybeHandlerMap = M.lookup t typeMap
        handlerMap = maybe singletonHandlerMap (M.insert hId wrapper) maybeHandlerMap
        typeMap' = M.insert t handlerMap typeMap
    in (hId, HandlerMap nextId' idMap' typeMap')

peekNextId :: HandlerMap world -> HandlerId
peekNextId (HandlerMap nextId _ _) = HandlerId nextId

delete :: HandlerId -> HandlerMap world -> HandlerMap world
delete hId m@(HandlerMap nextId idMap typeMap) =
    let maybeType = M.lookup hId idMap
        idMap' = M.delete hId idMap
    in case maybeType of
        Nothing -> m
        Just t -> HandlerMap nextId idMap' $ M.adjust (M.delete hId) t typeMap

getAllForEvent :: (Typeable world, EventData ev) => ev -> HandlerMap world
                                                       -> [Handler world ev]
getAllForEvent event (HandlerMap _ _ typeMap) =
    let t = typeOf event
        maybeHandlerMap = M.lookup t typeMap
        unwrapHandler (HandlerWrapper _ h) = cast h
    in case maybeHandlerMap of
        Just handlerMap -> Data.Maybe.mapMaybe (unwrapHandler) $ M.elems handlerMap
        Nothing -> []
