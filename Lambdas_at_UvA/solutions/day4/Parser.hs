-- (C) 2016 softlang.org, Ralf Laemmel

module Parser where

import Text.Parsec
import Test.HUnit (runTestTT, Test(TestLabel, TestList), (~=?))

-- | Shorthand for the parser type
type Parser = Parsec String ()

-- | A parser for floats
float :: Parser Float
float = (\x y -> read (x ++ y)) <$> many1 digit <*> places
  where
    places = option "" ((:) <$> char '.' <*> many1 digit)

-- | Test cases
tests :: Test
tests =
  TestList [
    TestLabel "parse1" $ Right 1 ~=? parse float "test" "1",
    TestLabel "parse1.1" $ Right 1.1 ~=? parse float "test" "1.1"
  ]

-- | Run tests
main :: IO ()
main = do
    testresults <- runTestTT tests
    print testresults
