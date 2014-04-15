{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List (mapAccumR)
import Test.QuickCheck
import Data.Tuple (swap)

import DisEvSim.Common
import DisEvSim.HandlerMap
import DisEvSim.TestUtil

main :: IO ()
main = runQuickCheck $quickCheckAll

prop_fillAndEmpty :: [Int] -> Bool
prop_fillAndEmpty ints =
    let events = map TestEvent ints
        accFn acc e = swap $ insert e emptyHandler acc
        (fullMap, idList) = mapAccumR accFn emptyHandlers events
        emptiedMap = foldr delete fullMap idList
    in all null $ map (flip getAllForEvent emptiedMap) events
