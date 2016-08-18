-- (C) 2016 softlang.org, Ralf Laemmel

module BX where

import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | Hierarchically nested department structure with budget per department
data Company = Company Name [Department]
  deriving (Eq, Show)
data Department = Department Name Budget [Department]
  deriving (Eq, Show)
type Name = String
type Budget = Float

-- | A sample company before and after salary cut
sampleInput, sampleOutput :: Company
sampleInput =
  Company "Acme" [
    Department "HR" 42 [],
    Department "Dev" 0 [
      Department "VB Dev" 88 [],
      Department "C# Dev" 100 []
    ]
  ]
sampleOutput =
  Company "Acme" [
    Department "HR" 21 [],
    Department "Dev" 0 [
      Department "VB Dev" 44 [],
      Department "C# Dev" 50 []
    ]
  ]

-- | Node-labeled rose trees
data NLTree a = NLTree a [NLTree a]
  deriving (Eq, Show)

-- | NLTree is a functor
instance Functor NLTree where
  fmap f (NLTree x ts) = NLTree (f x) (fmap (fmap f) ts)

-- | A list of trees representing budgets of sampleInput
sampleTree :: [NLTree Float]
sampleTree = [
    NLTree 42 [],
    NLTree 0 [
      NLTree 88 [],
      NLTree 100 []
    ]
  ]

-- | Budget cut at the level of (lists of) trees
cut :: [NLTree Float] -> [NLTree Float]
cut = fmap (fmap (/2))

-- | "get" direction of bidirectional transformation
get :: Company -> [NLTree Float]
get = undefined

-- | "put" direction of bidirectional transformation
put :: [NLTree Float] -> Company -> Company
put = undefined
    
-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "get" $ sampleTree ~?= get sampleInput,
    TestLabel "GetPut" $ sampleInput ~?= put (get sampleInput) sampleInput,
    TestLabel "cut" $ sampleOutput ~?= put (cut (get sampleInput)) sampleInput
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
