-- (C) 2016 softlang.org, Ralf Laemmel

module NLTree where

import Data.Foldable (toList)
import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~=?))

-- | Node-labeled rose trees
data NLTree a = NLTree a [NLTree a]
  deriving (Eq, Show)

-- | NLTree is a functor
instance Functor NLTree where
  fmap f (NLTree x ts) = NLTree (f x) (fmap (fmap f) ts)

-- | NLTree is a foldable
instance Foldable NLTree where
  -- This is one possible definition of foldr for NLTree.
  -- foldr f z (NLTree x ts) = foldr f z (x : concat (fmap toList ts))
  -- It basically takes advantage of the possibility of converting trees to lists.
  -- We are seeking an alternative definition that directly operates on trees.
  foldr f z (NLTree x ts) = f x (foldr (\t z' -> foldr f z' t) z ts)

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "foldr1" $ 15 ~=? foldr (+) 0 input,
    TestLabel "foldr2" $ [1,2,3,4,5] ~=? foldr (:) [] input
  ]
 where
  input = 
    NLTree 1 [
      NLTree 2 [],
      NLTree 3 [NLTree 4 []],
      NLTree 5 []]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
