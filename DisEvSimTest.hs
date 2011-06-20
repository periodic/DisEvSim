module Main where

import DisEvSim

import System.Environment

main = do
    [dur] <- getArgs
    let (t, log, w) = {-# SCC "simcall" #-} simulate (defaultConfig { enableLog = False}) 1 [("sum", \e -> modW (+e) >> after 1 1)] 1 (read dur)
    print (t, log, w)
