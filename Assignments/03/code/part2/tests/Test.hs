-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Tests" [pNumTests]

pNumTests = testGroup "pNum test"
  [ testCase "1" $ parseString "1" @?= Right [SExp (Const (IntVal 1))]
  , testCase "0" $ parseString "0" @?= Right [SExp (Const (IntVal 0))]
  , testCase "-0" $ parseString "-0" @?= Right [SExp (Const (IntVal 0))]
  , testCase "-1" $ parseString "-1" @?= Right [SExp (Const (IntVal (-1)))]
  , testCase "* -01" $ parseString "-01" @?= Left "cannot parse"
  , testCase "* 01" $ parseString "01" @?= Left "cannot parse"
  , testCase "* 007" $ parseString "007" @?= Left "cannot parse"
  , testCase "* +7" $ parseString "+7" @?= Left "cannot parse"
  , testCase "   -1" $ parseString "   -1" @?= Right [SExp (Const (IntVal (-1)))]
  ]

-- tests = testGroup "Minimal tests" [
--   testCase "simple success" $
--     parseString "2 + two" @?=
--       Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
--   testCase "simple failure" $
--     -- avoid "expecting" very specific parse-error messages
--     case parseString "wow!" of
--       Left e -> return ()  -- any message is OK
--       Right p -> assertFailure $ "Unexpected parse: " ++ show p]
