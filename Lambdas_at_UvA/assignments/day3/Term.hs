-- (C) 2016 softlang.org, Ralf Laemmel

module Term where

import Data.Map (Map, empty, insertWith, fromList)
import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | Simple expression forms
data Expr
  = TRUE -- True taken by Haskell Prelude
  | FALSE -- False taken by Haskell Prelude
  | Zero -- 0
  | Succ Expr -- (+1)
  | Pred Expr -- (+(-1))
  | IsZero Expr -- (==0)
  | If Expr Expr Expr -- if ... then ... else
  deriving (Eq, Read, Show)

-- | Sample expression
sampleExpr :: Expr
sampleExpr = Pred (If (IsZero Zero) (Succ (Succ Zero)) Zero)

-- | Universal term representation
data TermRep = TermRep ConstrId [TermRep]
  deriving (Eq, Read, Show)
-- | Constructor identifiers
type ConstrId = String

-- | Sample expression in universal representation
sampleTermRep :: TermRep
sampleTermRep =
  TermRep "Pred" [
    TermRep "If" [
      TermRep "IsZero" [TermRep "Zero" []],
      TermRep "Succ" [TermRep "Succ" [TermRep "Zero" []]],
      TermRep "Zero" [] ]]

-- | A class for explosion and implosion
class Term a where
  explode :: a -> TermRep
  implode :: TermRep -> a

-- | Make Expr an instance of Term
instance Term Expr where
  explode = undefined
  implode = undefined

-- | Count constructors; use TermRep!
metric :: Term a => a -> Map ConstrId Int
metric = undefined

-- | Sample output of metric
sampleMetric :: Map ConstrId Int
sampleMetric = fromList [("If",1),("IsZero",1),("Pred",1),("Succ",2),("Zero",3)]

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "explode" $ sampleTermRep ~?= explode sampleExpr,
    TestLabel "implode" $ sampleExpr ~?= implode sampleTermRep,
    TestLabel "metric" $ sampleMetric ~?= metric sampleExpr    
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
