module DisEvSim.EventQueueTest where

import HUnit

test1 = TestCase $ assertBool "False" False

tests = testList [TestLabel "test1" test1]

main = runTestTT tests


