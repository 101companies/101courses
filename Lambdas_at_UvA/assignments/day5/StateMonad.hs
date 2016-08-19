-- (C) 2016 softlang.org, Ralf Laemmel

module StateMonad where

-- These are good hints at what is used to solve the problem.
import Prelude hiding (lookup)
import Data.Maybe (fromJust)
import Data.Map (Map, empty, insert, lookup)
import Control.Monad.State.Lazy (State, runState, get, modify)
import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | Expression forms
data Expr =
    Const Int -- Evaluate to the int
  | Add Expr Expr -- Addition on ints
  | Seq Expr Expr -- Evaluate expressions from left to right, return last value
  | Assign String Expr -- Assign value of expression to string (variable)
  | Var String -- Deref variable (retrieve value) 
  deriving (Eq, Show)

-- | A sample program
sampleProg :: Expr
sampleProg =
  Seq
    (Assign "x" (Const 41))
    (Add (Var "x") (Const 1))

-- | An evaluator for these expression forms
eval :: Expr -> State (Map String Int) Int
eval (Const i) = return i
eval (Add e1 e2) = eval e1 >>= \i1 -> eval e2 >>= \i2 -> return (i1+i2) 
eval (Seq e1 e2) = eval e1 >> eval e2
eval (Assign s e) = undefined
eval (Var i) = undefined

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "const" $ 42 ~?= fst (runState (eval (Const 42)) empty),
    TestLabel "seq" $ 42 ~?= fst (runState (eval (Seq (Const 77) (Const 42))) empty),
    TestLabel "assign" $ 42 ~?= fst (runState (eval sampleProg) empty)
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
