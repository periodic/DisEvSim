module DisEvSim.EventQueueProperties where

prop1 :: Char -> Bool
prop1 c = c == 'a'

main = quickCheck prop1
