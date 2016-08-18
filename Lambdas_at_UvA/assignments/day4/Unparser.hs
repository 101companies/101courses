-- (C) 2016 softlang.org, Ralf Laemmel

module Unparser where

import Text.PrettyPrint.HughesPJ
import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~?=))

-- | A pretty printer for lists that
-- | spreads out elements vertically
-- | and indents each element additionally
pp :: [String] -> Doc
pp = undefined

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "pp1" $ ["foo"] ~?= lines (show (pp ["foo"])),
    TestLabel "pp2" $ ["foo", " bar"] ~?= lines (show (pp ["foo", "bar"])),
    TestLabel "pp3" $ ["foo", " bar", "  zoo"] ~?= lines (show (pp ["foo", "bar", "zoo"]))
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
