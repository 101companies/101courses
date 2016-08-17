-- (C) 2016 softlang.org, Ralf Laemmel

module InsertionSort where

import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | Polymorphic linear search
search :: Eq a => [a] -> a -> Bool
{-
search [] _ = False
search (x:xs) y = x==y || search xs y
-}
search = undefined -- use foldl/r

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "searchTrue" $ True ~?= search input 1,
    TestLabel "searchFalse" $ False ~?= search input 5
  ]
 where
  input = [2,4,3,1,4]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
