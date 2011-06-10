module Main where

import DisEvSim
import System.Environment (getArgs)


main =
    do  [dur] <- getArgs
        print $ simulate 1 [(\e -> modW (+e) >> after 1 1)] 0 (read dur :: Double)
