module DisEvSim.Common where

import Data.DList (DList)
{-
import Data.Record.Label

import qualified Control.Category as Cat

get = getL
set = setL

a <.> b = (Cat..) a b 
-}

type Time = Double
type DTime = Double

type EventLog ev = DList (Time, ev)
