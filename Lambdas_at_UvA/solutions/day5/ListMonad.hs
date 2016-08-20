-- (C) 2016 softlang.org, Ralf Laemmel

module ListMonad where

import Control.Monad
import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~=?))

-- | Expression forms including non-deterministic choice
data Expr =
    Const Int -- Return the int as is
  | Add Expr Expr -- Add ints (all combinations thereof)
  | Choice Expr Expr -- Computer either values (in fact, both) 

-- | An evaluator for these expression forms using the list monad
eval :: Expr -> [Int]
eval (Const i) = return i
eval (Add e1 e2) = eval e1 >>= \i1 -> eval e2 >>= \i2 -> return (i1+i2)
eval (Choice e1 e2) = eval e1 `mplus` eval e2

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "evalSimple" $ [1] ~=? eval (Const 1),
    TestLabel "evalModest" $ [3] ~=? eval (Add (Const 1) (Const 2)),
    TestLabel "evalTough" $ [3, 4] ~=? eval (Add (Const 1) (Choice (Const 2) (Const 3)))
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
