-- (C) 2016 softlang.org, Ralf Laemmel

module EitherMonad where

import Control.Monad.Except (throwError)
import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~=?))

-- | Expression forms including non-deterministic choice
data Expr =
    IntConst Int -- Return the int as is
  | StringConst String -- Return the string as is
  | Add Expr Expr -- Add ints
  | Concat Expr Expr -- Concatenate strings

-- | An evaluator for these expression forms
eval :: Expr -> Either Int String
eval (IntConst i) = Left i
eval (StringConst s) = Right s
eval (Add e1 e2) =
  case (eval e1, eval e2) of
    (Left i1, Left i2) -> Left (i1+i2)
    _ -> undefined -- for operands of wrong types.
eval (Concat e1 e2) =
  case (eval e1, eval e2) of
    (Right s1, Right s2) -> Right (s1++s2)
    _ -> undefined -- for operands of wrong types.

-- | An evaluator with error handling in the Either monad.
-- | It returns error messages for operands of wrong types.
eval' :: Expr -> Either String (Either Int String)
eval' (IntConst i) = return $ Left i
eval' (StringConst s) = return $ Right s
eval' (Add e1 e2) = do
  v1 <- eval' e1
  v2 <- eval' e2
  case (v1, v2) of
    (Left i1, Left i2) -> return $ Left (i1+i2)
    _ -> throwError "add on string"
eval' (Concat e1 e2) = do
  v1 <- eval' e1
  v2 <- eval' e2
  case (v1, v2) of
    (Right s1, Right s2) -> return $ Right (s1++s2)
    _ -> throwError "concat on int"

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "add" $ Left 42 ~=? eval (Add (IntConst 20) (IntConst 22)),
    TestLabel "concat" $ Right "42" ~=? eval (Concat (StringConst "4") (StringConst "2")),
    TestLabel "add" $ Right (Left 42) ~=? eval' (Add (IntConst 20) (IntConst 22)),
    TestLabel "concat" $ Right (Right "42") ~=? eval' (Concat (StringConst "4") (StringConst "2")),
    TestLabel "addError" $ Left "add on string" ~=? eval' (Add (IntConst 0) (StringConst "")),
    TestLabel "concatError" $ Left "concat on int" ~=? eval' (Concat (IntConst 0) (StringConst ""))
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
