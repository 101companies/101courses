-- (C) 2016 softlang.org, Ralf Laemmel

module SelectionSort where

import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | Polymorphic sorting
sort :: Ord a => [a] -> [a]
sort xs = selects xs

-- | Repeat selection of smallest element
selects :: Ord a => [a] -> [a]
selects [] = []
selects xs = x : selects xs'
  where
    x = smallest (head xs) (tail xs)
    xs' = remove x xs

-- | Find smallest element
smallest :: Ord a => a -> [a] -> a
{-
smallest x [] = x
smallest x (y:ys) = smallest (min x y) ys
-}
smallest = undefined -- use foldl/r

-- | Remove a given element
remove :: Eq a => a -> [a] -> [a]
remove _ [] = error "Element not found for removal."
remove x (y:ys) =
  if x==y
    then ys
    else y : remove x ys

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
