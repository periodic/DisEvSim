module DisEvSim.Handler where

import DisEvSim.Common

import Data.Map

makeHandlerId = HandlerId

addHandler :: String -> Handler world ev -> HandlerMap world ev -> HandlerMap world ev
addHandler = insert . makeHandlerId

handlersFromList :: [(String, Handler world ev)] -> HandlerMap world ev
handlersFromList = fromList . Prelude.map (\(k,v) -> (makeHandlerId k, v))

processHandlers :: ev -> HandlerMap world ev -> Sim world ev ()
processHandlers ev map = let actions = {-# SCC "AccumulateActions" #-} Data.Map.map (\h -> h ev) map
                         in {-# SCC "CreateWorld" #-} Data.Map.fold (>>) (return ()) actions