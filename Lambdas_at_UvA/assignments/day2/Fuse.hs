-- (C) 2016 softlang.org, Ralf Laemmel

module Fuse where

import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~=?))

-- | fuse takes two lists and combines them by taking elements alternatingly.
-- | For example: fuse [1,2,3] [4,5,6] evaluates to [1,4,2,5,3,6].
-- | Do not use non-trivial functions on lists.
fuse :: [Int] -> [Int] -> [Int]
fuse = undefined
    
-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "fuse1" $ fuse [1,2,3] [] ~=? [1,2,3],
    TestLabel "fuse2" $ fuse [1,2,3] [4,5,6] ~=? [1,4,2,5,3,6],
    TestLabel "fuse3" $ fuse [1,2] [4,5,6] ~=? [1,4,2,5,6]
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
