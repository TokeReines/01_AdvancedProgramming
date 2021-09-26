-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Tests" [pNumTests, pStrTest, pIdentTest]

assertFailure' a = case a of 
  Left e -> return ()
  Right p -> assertFailure $ "Unexpected parse: " ++ show p

pNumTests = testGroup "pNum tests"
  [ testCase "1" $ parseString "1" @?= Right [SExp (Const (IntVal 1))]
  , testCase "0" $ parseString "0" @?= Right [SExp (Const (IntVal 0))]
  , testCase "-0" $ parseString "-0" @?= Right [SExp (Const (IntVal 0))]
  , testCase "-1" $ parseString "-1" @?= Right [SExp (Const (IntVal (-1)))]
  , testCase "* -01" $ case parseString "-01" of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p
  , testCase "* 01" $ assertFailure' $ parseString "01"
  , testCase "* 007" $  assertFailure' $ parseString "007"  
  , testCase "* +7" $ assertFailure' $ parseString "+7"
  , testCase "   -1" $ parseString "   -1" @?= Right [SExp (Const (IntVal (-1)))]
  ]

pStrTest = testGroup "pStr tests"
  [ testCase "'fo\\\\o\\\nb\\\n\\\'" $ parseString "'fo\\\\o\\\nb\na\\\'r'" @?= Right [SExp (Const (StringVal "fo\\ob\na'r"))]
  , testCase "* '\\t'" $ assertFailure' $ parseString "'\\t'"
  , testCase "'\\\\'" $ parseString "'\\\\'" @?= Right [SExp (Const (StringVal "\\"))]  
  , testCase "'a\nb'" $ parseString "'a\nb'" @?= Right [SExp (Const (StringVal "a\nb"))]  
  , testCase "'a\\\nb'" $ parseString "'a\\\nb'" @?= Right [SExp (Const (StringVal "ab"))]  
  , testCase "x='Hello World!'" $ parseString "x='Hello World!'" @?= Right [SDef "x" (Const (StringVal "Hello World!"))]
  ]

pIdentTest = testGroup "pIdent tests"
  [ testCase "x=2" $ parseString "x=2" @?= Right [SDef "x" (Const (IntVal 2))]
  , testCase "OneTo5=2" $  parseString "OneTo5=2" @?= Right [SDef "OneTo5" (Const (IntVal 2))]
  , testCase "_One_To5_=2" $ parseString "_One_To5_=2" @?= Right [SDef "_One_To5_" (Const (IntVal 2))]
  , testCase "  \t\n_One_To5_  \t\n=  \t\n2  \t\n" $ parseString "  \t\n_One_To5_  \t\n=  \t\n2  \t\n" @?= Right [SDef "_One_To5_" (Const (IntVal 2))]
  , testCase "* 1to5=2" $ assertFailure' $ parseString "1to5=2"
  , testCase "* None=2" $ assertFailure' $  parseString "None=2"
  , testCase "* True=2" $ assertFailure' $  parseString "True=2"
  , testCase "* False=2" $ assertFailure' $  parseString "False=2"
  , testCase "* for=2" $ assertFailure' $  parseString "for=2"
  , testCase "* if=2" $ assertFailure' $  parseString "if=2"
  , testCase "* in=2" $ assertFailure' $  parseString "in=2"
  , testCase "* not=2" $ assertFailure' $  parseString "not=2"
  ]

pNoneTrueFalseTest = testGroup "pNoneTrueFalseTest"
  [ testCase "x=None" $ parseString "x=None" @?= Right [SDef "x" (Const NoneVal)]
  , testCase "x=True" $ parseString "x=True" @?= Right [SDef "x" (Const TrueVal)]
  , testCase "x=False" $ parseString "x=False" @?= Right [SDef "x" (Const FalseVal)]
  , testCase "  \t\nx  \t\n=  \t\nFalse  \t\n" $ parseString "  \t\nx  \t\n=  \t\nFalse  \t\n" @?= Right [SDef "x" (Const FalseVal)]
  , testCase "* x=none" $ assertFailure' $ parseString "x=none"
  , testCase "* x=true" $ assertFailure' $ parseString "x=true"
  , testCase "* x=false" $ assertFailure' $ parseString "x=false"

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
