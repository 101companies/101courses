-- (C) 2016 softlang.org, Ralf Laemmel

module InsertionSort where

import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | Polymorphic sorting
sort :: Ord a => [a] -> [a]
sort xs = inserts xs []

-- | Insert given elements in an emerging result
inserts :: Ord a => [a] -> [a] -> [a]
{-
inserts [] r = r
inserts (x:xs) r = inserts xs (insert x r)
-}
inserts = undefined -- use foldl/r

-- | Insert a given element in a list
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) =
  if x <= y
    then x : y : ys
    else y : insert x ys

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "sort0" $ ([]::[Int]) ~?= sort ([]::[Int]),
    TestLabel "sort1" $ [42] ~?= sort [42],
    TestLabel "sort2" $ [42,88] ~?= sort [42,88],
    TestLabel "sort3" $ [1,2,3] ~?= sort [1,3,2]
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
