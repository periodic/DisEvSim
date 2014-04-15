{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck

import DisEvSim.TestUtil

main :: IO ()
main = runQuickCheck $quickCheckAll

