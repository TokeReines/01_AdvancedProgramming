-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Tests" [numTests, strTest, identTest, minimalTests, exprOperExprTest, notTest]

assertFailure' a = case a of 
  Left e -> return ()
  Right p -> assertFailure $ "Unexpected parse: " ++ show p

numTests = testGroup "pNum tests"
  [ testCase "1" $ parseString "1" @?= Right [SExp (Const (IntVal 1))]
  , testCase "0" $ parseString "0" @?= Right [SExp (Const (IntVal 0))]
  , testCase "-0" $ parseString "-0" @?= Right [SExp (Const (IntVal 0))]
  , testCase "-1" $ parseString "-1" @?= Right [SExp (Const (IntVal (-1)))]
  , testCase "* -01" $ case parseString "-01" of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p
  , testCase "* 01" $ assertFailure' $ parseString "01"
  , testCase "* 007" $  assertFailure' $ parseString "007"  
  , testCase "* +7" $ assertFailure' $ parseString "+7"
  , testCase "* --7" $ assertFailure' $ parseString "--7"
  , testCase "   -1" $ parseString "   -1" @?= Right [SExp (Const (IntVal (-1)))]
  ]

strTest = testGroup "pStr tests"
  [ testCase "'a\\\n b\\n\\\nc\\\n\\nd'" $ parseString "'a\\\n b\\n\\\nc\\\n\\nd'" @?= Right [SExp (Const (StringVal "a b\nc\nd"))]
  , testCase "* '\\t'" $ assertFailure' $ parseString "'\\t'"
  , testCase "'\\\\'" $ parseString "'\\\\'" @?= Right [SExp (Const (StringVal "\\"))]  
  , testCase "'a\nb'" $ parseString "'a\nb'" @?= Right [SExp (Const (StringVal "a\nb"))]  
  , testCase "'a\\\nb'" $ parseString "'a\\\nb'" @?= Right [SExp (Const (StringVal "ab"))]  
  , testCase "x='Hello World!'" $ parseString "x='Hello World!'" @?= Right [SDef "x" (Const (StringVal "Hello World!"))]
  ]

noneTrueFalseTest = testGroup "pNoneTrueFalseTest"
  [ testCase "x=None" $ parseString "x=None" @?= Right [SDef "x" (Const NoneVal)]
  , testCase "x=True" $ parseString "x=True" @?= Right [SDef "x" (Const TrueVal)]
  , testCase "x=False" $ parseString "x=False" @?= Right [SDef "x" (Const FalseVal)]
  , testCase "  \t\nx  \t\n=  \t\nFalse  \t\n" $ parseString "  \t\nx  \t\n=  \t\nFalse  \t\n" @?= Right [SDef "x" (Const FalseVal)]
  , testCase "* x=none" $ assertFailure' $ parseString "x=none"
  , testCase "* x=true" $ assertFailure' $ parseString "x=true"
  , testCase "* x=false" $ assertFailure' $ parseString "x=false"

  ]

identTest = testGroup "pIdent tests"
  [ testCase "x=2" $ parseString "x=2" @?= Right [SDef "x" (Const (IntVal 2))]
  , testCase "x=10//5" $ parseString "x=10//5" @?= Right [SDef "x" (Oper Div (Const (IntVal 10)) (Const (IntVal 5)))]
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

exprOperExprTest = testGroup "Expr Oper Expr"
  -- Test operators first
  [ testCase "1+1" $ parseString "1+1" @?= Right [SExp (Oper Plus (Const (IntVal 1)) (Const (IntVal 1)))]
  , testCase "1-1" $ parseString "1-1" @?= Right [SExp (Oper Minus (Const (IntVal 1)) (Const (IntVal 1)))]
  , testCase "1*1" $ parseString "1*1" @?= Right [SExp (Oper Times (Const (IntVal 1)) (Const (IntVal 1)))]
  , testCase "1//1" $ parseString "1//1" @?= Right [SExp (Oper Div (Const (IntVal 1)) (Const (IntVal 1)))]
  , testCase "1%1" $ parseString "1%1" @?= Right [SExp (Oper Mod (Const (IntVal 1)) (Const (IntVal 1)))]
  , testCase "1==1" $ parseString "1==1" @?= Right [SExp (Oper Eq (Const (IntVal 1)) (Const (IntVal 1)))]
  , testCase "1!=1" $ parseString "1!=1" @?= Right [SExp (Not (Oper Eq (Const (IntVal 1)) (Const (IntVal 1))))]
  , testCase "1<1" $ parseString "1<1" @?= Right [SExp (Oper Less (Const (IntVal 1)) (Const (IntVal 1)))]  
  , testCase "1<=1" $ parseString "1<=1" @?= Right [SExp (Not (Oper Greater (Const (IntVal 1)) (Const (IntVal 1))))]
  , testCase "1>1" $ parseString "1>1" @?= Right [SExp (Oper Greater (Const (IntVal 1)) (Const (IntVal 1)))]  
  , testCase "1>=1" $ parseString "1>=1" @?= Right [SExp (Not (Oper Less (Const (IntVal 1)) (Const (IntVal 1))))] 
  , testCase "1 in 1" $ parseString "1 in 1" @?= Right [SExp (Oper In (Const (IntVal 1)) (Const (IntVal 1)))]  
  , testCase "1 not in 1" $ parseString "1 not in 1" @?= Right [SExp (Not (Oper In (Const (IntVal 1)) (Const (IntVal 1))))]
  -- Test Left Associativity ((1+2)+3)
  , testCase "1+2+3" $ parseString "1+2+3" @?= Right [SExp (Oper Plus (Oper Plus (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))] 
  -- Test precedende level of operators
  , testCase "1+2*3" $ parseString "1+2*3" @?= Right [SExp (Oper Plus (Const (IntVal 1)) (Oper Times (Const (IntVal 2)) (Const (IntVal 3))))] 
  -- Test precedende level of operators and associativity (1+((2*3)/4))
  , testCase "1+2*3//4" $ parseString "1+2*3//4" @?= Right [SExp (Oper Plus (Const (IntVal 1)) (Oper Div (Oper Times (Const (IntVal 2)) (Const (IntVal 3))) (Const (IntVal 4))))]
  -- Test lt/gt
  ]

notTest = testGroup "not Expr"
  [ testCase "not True" $ parseString "not True" @?= Right [SExp (Not (Const TrueVal))]
  , testCase "not 1" $ parseString "not 1" @?= Right [SExp (Not (Const (IntVal 1)))]
  ]

minimalTests = testGroup "Minimal tests" [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
      
  testCase "Big program" $
    parseString "x = (2, [y*2 for y in range(1,10,2) if y > 5]) # Some comment" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]
