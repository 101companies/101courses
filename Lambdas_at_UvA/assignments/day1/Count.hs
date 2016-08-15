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

module Count where

import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | count c s returns the number of times c occurs in s.
-- | For example: count 'x' "xyz" == 1
count :: Char -> [Char] -> Int
count = undefined
    
-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "count0" $ count 'x' "abc" ~?= 0,
    TestLabel "count1" $ count 'x' "xyz" ~?= 1,
    TestLabel "count3" $ count 'a' "reallynotabba" ~?= 3
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
