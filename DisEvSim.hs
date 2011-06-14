{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module DisEvSim ( simulate
                , getW
                , putW
                , modW
                , getT
                , after
                , addHandler
                -- Types
                , Time
                , DTime
                , Handler
                , Sim
                ) where

import DisEvSim.Common
import DisEvSim.EventQueue
import DisEvSim.Internal
import DisEvSim.Debug

