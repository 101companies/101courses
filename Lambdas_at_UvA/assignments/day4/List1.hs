-- (C) 2016 softlang.org, Ralf Laemmel

module List1 where

import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | Lists that cannot be empty
data List1 a = One a | More a (List1 a)
  deriving (Eq, Show)

-- | List1 is a functor
instance Functor List1 where
  fmap = undefined

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "fmap1" $ output1 ~?= fmap (+1) input,
    TestLabel "fmap2" $ output2 ~?= fmap odd input
  ]
 where
  input = More 1 (More 2 (One 3))
  output1 = More 2 (More 3 (One 4))
  output2 = More True (More False (One True))

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
