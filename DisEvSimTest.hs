module Main where

import DisEvSim

import System.Environment

data Counter = Counter { total :: !Integer
                       , count :: !Integer
                       } deriving Show

main = do
    [dur] <- getArgs
    let handlers = [ ("sum", \e -> modW (\w@(Counter s _) -> w { total = s + e }) >> after 1 (e + 1))
                   , ("sub", \e -> modW (\w@(Counter _ c) -> w { count = c + 1 }))
                   ]
        initial  = Counter 0 0
        (t, log, w) = {-# SCC "simcall" #-} simulate (defaultConfig { enableLog = False}) initial handlers 1 (read dur)
    print (t, log, w)
