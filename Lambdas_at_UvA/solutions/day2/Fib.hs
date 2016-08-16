-- (C) 2016 softlang.org, Ralf Laemmel

module Fib where

import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | fibseq n returns the Fibonacci sequence of length n.
-- | For example: fibseq 5 evaluates to [0,1,1,2,3].
fibseq :: Int -> [Int]
fibseq 0 = []
fibseq n | n > 0 = fibseq (n-1) ++ [fib (n-1)]

-- | The regular fib function may be of use, but that's just an option.
fib 0 = 0
fib 1 = 1
fib n | n > 1 = fib (n-1) + fib (n-2)

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "fibseq0" $ [] ~?= fibseq 0,
    TestLabel "fibseq1" $ [0] ~?= fibseq 1,
    TestLabel "fibseq5" $ [0,1,1,2,3] ~?= fibseq 5,
    TestLabel "fibseq8" $ [0,1,1,2,3,5,8,13] ~?= fibseq 8
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
