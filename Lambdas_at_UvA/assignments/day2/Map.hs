-- (C) 2016 softlang.org, Ralf Laemmel

module Map where

import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | function m is True, if the first components of m are distinct.
-- | Hint: you are likely to need a helper function.
-- | Do not use non-trivial functions on lists.
function :: [(Char, Char)] -> Bool
function = undefined

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "function1" $ False ~?= function [('a','a'), ('a', 'b')],
    TestLabel "function2" $ True ~?= function [('a','a'), ('b', 'b')]
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
