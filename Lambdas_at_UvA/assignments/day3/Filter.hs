-- (C) 2016 softlang.org, Ralf Laemmel

module Filter where

import Prelude hiding (filter)
import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | The filter function known from the Prelude
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
-- Define the non-empty case in a way to obey two constraints.
-- 1. There is only one recursive occurrence of filter. Thus, this is NOT OK:
-- filter p (x:xs) = if p x then x : filter p xs else filter p xs
-- 2. Local scope (let and where) is not used. Thus, this is NOT OK:
-- filter p (x:xs) = if p x then x : r else r where r = filter p xs
-- 3. There is no performance penalty due to extra list processing. Thus, this is NOT OK:
-- filter p (x:xs) = (if p x then [x] else []) ++ filter p xs
filter p (x:xs) = undefined

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "filterOdd" $ [1,3,5] ~?= filter odd [0..5],
    TestLabel "filterEven" $ [0,2,4] ~?= filter even [0..5]
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
