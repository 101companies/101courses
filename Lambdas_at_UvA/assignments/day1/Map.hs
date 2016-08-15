{-

(C) 2016 softlang.org, Ralf Laemmel

See module Fib.hs for a general guide.

-}

module Map where

import Test.HUnit (runTestTT,Test(TestLabel,TestList),(~?=))

-- | function m is True, if the first components of m are distinct.
function :: [(Char, Char)] -> Bool
function = undefined

-- | injective m is True, if the second components of m are distinct.
injective :: [(Char, Char)] -> Bool
injective = undefined
    
-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "function1" $ function [('a','a'), ('a', 'b')] ~?= False,
    TestLabel "function2" $ function [('a','a'), ('b', 'b')] ~?= True,
    TestLabel "injective1" $ injective [('a','b'), ('b', 'b')] ~?= False,
    TestLabel "injective2" $ injective [('a','b'), ('b', 'a')] ~?= True
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
