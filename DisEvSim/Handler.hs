module DisEvSim.Handler where

import Control.Lens
--import Control.Applicative
import Control.Monad.State
--import Data.Map

import DisEvSim.Common

getNewHandlerId :: Sim world HandlerId
getNewHandlerId = do
    st <- get
    let st' = st & nextHandlerId +~ 1
    put st'
    return . HandlerId $ st' ^. nextHandlerId

{-
insertHandler :: String -> Handler world ev -> HandlerMap world ev -> HandlerMap world ev
insertHandler = insert . makeHandlerId

deleteHandler :: String -> HandlerMap world ev -> HandlerMap world ev
deleteHandler = delete . makeHandlerId

handlersFromList :: [(String, Handler world ev)] -> HandlerMap world ev
handlersFromList = fromList . Prelude.map (\(k,v) -> (makeHandlerId k, v))

processHandlers :: ev -> HandlerMap world ev -> Sim world ev ()
processHandlers ev map = let actions = {-# SCC "AccumulateActions" #-} Data.Map.map (\h -> h ev) map
                         in {-# SCC "CreateWorld" #-} Data.Map.fold (>>) (return ()) actions
-}
