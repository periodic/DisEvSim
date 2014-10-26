{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List (mapAccumR)
import Data.Tuple (swap)
import Test.QuickCheck.All

import DisEvSim.Common
import DisEvSim.HandlerMap
import DisEvSim.TestUtil

main :: IO ()
main = runQuickCheck $quickCheckAll

-- | Tests that for any set of events the map will accept a list of handlers
--   and when removed will no longer return those handlers.
prop_fillAndEmpty :: [Int] -> Bool
prop_fillAndEmpty ints =
    let events = map TestEvent ints
        accFn acc e = swap $ insert e emptyHandler acc
        (fullMap, idList) = mapAccumR accFn defaultHandlerMap events
        emptiedMap = foldr delete fullMap idList
    in all null $ map (flip getAllForEvent emptiedMap) events
