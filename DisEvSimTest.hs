module Main where

import DisEvSim

import System.Environment

main = do
    [dur] <- getArgs
    let (t, _, w) = {-# SCC "simcall" #-} simulate 1 [("sum", \e -> modW (+e) >> after 1 1)] 1 (read dur)
    print (t, w)
