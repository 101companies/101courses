-- (C) 2016 softlang.org, Ralf Laemmel

module Term where

import Data.Map (Map, empty, insertWith, fromList)
import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~=?))

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
  explode TRUE = TermRep "TRUE" []
  explode FALSE = TermRep "FALSE" []
  explode Zero = TermRep "Zero" []
  explode (Succ e) = TermRep "Succ" [explode e]
  explode (Pred e) = TermRep "Pred" [explode e]
  explode (IsZero e) = TermRep "IsZero" [explode e]
  explode (If e0 e1 e2) = TermRep "If" (map explode [e0, e1, e2])
  implode (TermRep "TRUE" []) = TRUE
  implode (TermRep "FALSE" []) = FALSE
  implode (TermRep "Zero" []) = Zero
  implode (TermRep "Succ" [t]) = Succ (implode t)
  implode (TermRep "Pred" [t]) = Pred (implode t)
  implode (TermRep "IsZero" [t]) = IsZero (implode t)
  implode (TermRep "If" [t0, t1, t2]) = If e0 e1 e2
    where
      e0 = implode t0
      e1 = implode t1
      e2 = implode t2

-- | Count constructors; use TermRep!
metric :: Term a => a -> Map ConstrId Int
metric t = f empty $ explode t
  where
    f m (TermRep c rs) = foldl f (insertWith (+) c 1 m) rs

-- | Sample output of metric
sampleMetric :: Map ConstrId Int
sampleMetric = fromList [("If",1),("IsZero",1),("Pred",1),("Succ",2),("Zero",3)]

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "explode" $ sampleTermRep ~=? explode sampleExpr,
    TestLabel "implode" $ sampleExpr ~=? implode sampleTermRep,
    TestLabel "metric" $ sampleMetric ~=? metric sampleExpr    
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
