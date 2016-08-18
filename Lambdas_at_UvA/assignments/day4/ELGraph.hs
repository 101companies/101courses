-- (C) 2016 softlang.org, Ralf Laemmel

module ELGraph where

import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | An edge-labled graph
newtype ELGraph a = ELGraph [Edge a]
  deriving (Eq, Show)
type Edge a = (String, String, a)

-- | ELGraph is a functor
instance Functor ELGraph where
  fmap = undefined
  
-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "fmap1" $ output1 ~?= fmap id input,
    TestLabel "fmap2" $ output2 ~?= fmap (const ()) input
  ]
 where
  input = ELGraph [("a", "b", 21), ("b", "c", 20), ("c", "a", 1)]
  output1 = ELGraph [("a", "b", 21), ("b", "c", 20), ("c", "a", 1)]
  output2 = ELGraph [("a", "b", ()), ("b", "c", ()), ("c", "a", ())]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
