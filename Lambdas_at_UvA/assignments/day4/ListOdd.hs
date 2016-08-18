-- (C) 2016 softlang.org, Ralf Laemmel

module ListOdd where

import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | Lists that hold an odd number of elements
data ListOdd a = One a | More a a (ListOdd a)
  deriving (Eq, Show)

-- | ListOdd is a functor
instance Functor ListOdd where
  fmap f (One x) = One (f x)
  fmap f (More x y l) = More (f x) (f y) (fmap f l)

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "fmap1" $ output1 ~?= fmap (+1) input,
    TestLabel "fmap2" $ output2 ~?= fmap odd input
  ]
 where
  input = More 1 2 (More 3 4 (One 5))
  output1 = More 2 3 (More 4 5 (One 6))
  output2 = More True False (More True False (One True))

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
