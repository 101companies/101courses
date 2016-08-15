{-

(C) 2016 softlang.org, Ralf Laemmel

A general guide: We use "undefined" to indicate a missing
implementation; this is what you need to work on. Have a look at the
description (the comments) and the test cases provided with the
module. You should work GHC(i) version 8.*.*. Your solution should
pass the test case provided. You are allowed to create additional
functions, but you are not supposed to change the signature of any
given function or the header of the module. Please try to use UTF8
encoding for your files.

-}

module Fib where

import Test.HUnit (runTestTT,Test(TestLabel,TestList),(~?=))

-- | fibseq n returns the Fibonacci sequence of length n.
-- | For example: fibseq 5 == [0,1,1,2,3]
fibseq :: Int -> [Int]
fibseq n = undefined
    
-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "fibseq1" $ fibseq 1 ~?= [0],
    TestLabel "fibseq5" $ fibseq 5 ~?= [0,1,1,2,3],
    TestLabel "fibseq8" $ fibseq 8 ~?= [0,1,1,2,3,5,8,13]
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
