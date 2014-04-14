{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List (nub)
import Test.QuickCheck

import DisEvSim.Common
import DisEvSim.Handler
import DisEvSim.TestUtil

main :: IO ()
main = runQuickCheck $quickCheckAll

prop_AlwaysNewId :: Positive Int -> Bool
prop_AlwaysNewId (Positive calls) =
    let actions = sequence $ replicate calls getNewHandlerId
        ids = evalSim defaultConfig (defaultState 0 :: SimState Int) actions
    in (length $ nub ids) == calls
