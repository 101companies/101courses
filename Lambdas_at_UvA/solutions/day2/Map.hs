-- (C) 2016 softlang.org, Ralf Laemmel

module Map where

import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | function m is True, if the first components of m are distinct.
-- | Hint: you are likely to need a helper function.
-- | Do not use non-trivial functions on lists.
function :: [(Char, Char)] -> Bool
function [] = True
function ((x,_):f) = not (clash x f)
  where
    clash _ [] = False
    clash x ((x',_):f) = x==x' || clash x f

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
