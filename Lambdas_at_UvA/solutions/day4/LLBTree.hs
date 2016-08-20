-- (C) 2016 softlang.org, Ralf Laemmel

module LLBTree where

import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~=?))

-- | Leaf-labeled binary trees
data LLBTree a = Leaf a | Fork (LLBTree a) (LLBTree a)
  deriving (Eq, Show)

-- | LLBTree is a functor
instance Functor LLBTree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Fork l r) = Fork (fmap f l) (fmap f r)

-- | LLBTree is a foldable
instance Foldable LLBTree where
  foldr f z (Leaf x) = f x z
  foldr f z (Fork l r) = foldr f (foldr f z r) l

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "foldr1" $ 10 ~=? foldr (+) 0 input,
    TestLabel "foldr2" $ [1,2,3,4] ~=? foldr (:) [] input
  ]
 where
  input = Fork (Leaf 1) (Fork (Leaf 2) (Fork (Leaf 3) (Leaf 4)))

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
