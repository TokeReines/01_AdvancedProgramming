-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Tests" [stmtsTests, numTests, strTest, identTest, noneTrueFalseTest,  exprOperExprTest, notTest, parenthesisTest, identPExprzPTest, sqBrackTest, forClauseTest, commentsTest ,minimalTests]

assertFailure' a = case a of 
  Left e -> return ()
  Right p -> assertFailure $ "Unexpected parse: " ++ show p

stmtsTests = testGroup "stmts tests"
  [ testCase "1" $ parseString "1" @?= Right [SExp (Const (IntVal 1))]
  , testCase "x=1" $ parseString "x=1" @?= Right [SDef "x" (Const (IntVal 1))]
  , testCase "1;1" $ parseString "1;1" @?= Right [SExp (Const (IntVal 1)), SExp (Const (IntVal 1))]
  , testCase "1;x=1" $ parseString "1;x=1" @?= Right [SExp (Const (IntVal 1)),SDef "x" (Const (IntVal 1))]
  , testCase "* 1;x=1;" $ assertFailure' $ parseString "1;x=1;"
  ]

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
  , testCase "   -1    " $ parseString "   -1" @?= Right [SExp (Const (IntVal (-1)))]
  ]

strTest = testGroup "pStr tests"
  [ testCase "'a\\\n b\\n\\\nc\\\n\\nd'" $ parseString "'a\\\n b\\n\\\nc\\\n\\nd'" @?= Right [SExp (Const (StringVal "a b\nc\nd"))]
  , testCase "* '\\t'" $ assertFailure' $ parseString "'\\t'"
  , testCase "* '\n'" $ assertFailure' $ parseString "'\n'"
  , testCase "* '\t'" $ assertFailure' $ parseString "'\t'"
  , testCase "''" $ parseString "''" @?= Right [SExp (Const (StringVal ""))]
  , testCase "\t\n'Hello World'\t\n" $ parseString "\t\n'Hello World'\t\n" @?= Right [SExp (Const (StringVal "Hello World"))]
  , testCase "'\\\\'" $ parseString "'\\\\'" @?= Right [SExp (Const (StringVal "\\"))]  
  , testCase "* 'a\nb'" $ assertFailure' $ parseString "'a\nb'"
  , testCase "'a\\\nb'" $ parseString "'a\\\nb'" @?= Right [SExp (Const (StringVal "ab"))]  
  , testCase "x='Hello World!'" $ parseString "x='Hello World!'" @?= Right [SDef "x" (Const (StringVal "Hello World!"))]
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

noneTrueFalseTest = testGroup "pNoneTrueFalseTest"
  [ testCase "x=None" $ parseString "x=None" @?= Right [SDef "x" (Const NoneVal)]
  , testCase "x=True" $ parseString "x=True" @?= Right [SDef "x" (Const TrueVal)]
  , testCase "x=False" $ parseString "x=False" @?= Right [SDef "x" (Const FalseVal)]
  , testCase "  \t\nx  \t\n=  \t\nFalse  \t\n" $ parseString "  \t\nx  \t\n=  \t\nFalse  \t\n" @?= Right [SDef "x" (Const FalseVal)]
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
  , testCase  "2+2<1+1" $ parseString "2+2<1+1" @?= Right [SExp (Oper Less (Oper Plus (Const (IntVal 2)) (Const (IntVal 2))) (Oper Plus (Const (IntVal 1)) (Const (IntVal 1))))]
  , testCase "1+not 1" $ assertFailure' $ parseString "1+not 1"
  ]

notTest = testGroup "not Expr"
  [ testCase "not True" $ parseString "not True" @?= Right [SExp (Not (Const TrueVal))]
  , testCase "not 1" $ parseString "not 1" @?= Right [SExp (Not (Const (IntVal 1)))]
  , testCase "not not 1" $ parseString "not not 1" @?= Right [SExp (Not (Not (Const (IntVal 1))))]
  ]

parenthesisTest = testGroup "parenthesis Test" 
  [ testCase "(1)" $ parseString "(1)" @?= Right [SExp (Const (IntVal 1))]
  , testCase "(  \n  \t 1  \n \t  )" $ parseString "(  \n  \t 1  \n \t  )" @?= Right [SExp (Const (IntVal 1))]
  , testCase "('Hello World')" $ parseString "('Hello World')" @?= Right [SExp (Const (StringVal "Hello World"))]
  , testCase "(_1hello_world_)" $ parseString "(_1hello_world_)" @?= Right [SExp (Var "_1hello_world_")]
  , testCase "(None)" $ parseString "(None)" @?= Right [SExp (Const NoneVal)]
  , testCase "(True)" $ parseString "(True)" @?= Right [SExp (Const TrueVal)]
  , testCase "(False)" $ parseString "(False)" @?= Right [SExp (Const FalseVal)]
  , testCase "(not False)" $ parseString "(not False)" @?= Right [SExp (Not (Const FalseVal))]
  , testCase "((1))" $ parseString "((1))" @?= Right [SExp (Const (IntVal 1))]
  , testCase "(1+1)" $ parseString "(1+1)" @?= Right [SExp (Oper Plus (Const (IntVal 1)) (Const (IntVal 1)))]
  , testCase "(1<1)" $ parseString "(1<1)" @?= Right [SExp (Oper Less (Const (IntVal 1)) (Const (IntVal 1)))]
  , testCase "(((1)))" $ parseString "(((1)))" @?= Right [SExp (Const (IntVal 1))]
  , testCase "(range(10,2,3))" $ parseString "(range(10,2,3))" @?= Right [SExp (Call "range" [Const (IntVal 10),Const (IntVal 2),Const (IntVal 3)])]
  , testCase "([1,x,[1,2],'Hello World!'])" $ parseString "([1,x,[1,2],'Hello World!'])" @?= Right [SExp (List [Const (IntVal 1),Var "x",List [Const (IntVal 1),Const (IntVal 2)],Const (StringVal "Hello World!")])]
  , testCase "([x for x in z if b < y])" $ parseString "([x for x in z if b < y])" @?= Right [SExp (Compr (Var "x") [CCFor "x" (Var "z"),CCIf (Oper Less (Var "b") (Var "y"))])]
  ]

identPExprzPTest = testGroup "ident Paren Exprz Paren Test"
  [ testCase "range(1,2,3)" $ parseString "range(1,2,3)" @?= Right [SExp (Call "range" [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3)])]
  , testCase "print(1,2,3)" $ parseString "print(1,2,3)" @?= Right [SExp (Call "print" [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3)])]
  , testCase "print()" $ parseString "print()" @?= Right [SExp (Call "print" [])]
  , testCase "print \n \t (1,2,3)" $ parseString "print \n \t (1,2,3)" @?= Right [SExp (Call "print" [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3)])]
  , testCase "True(1,2,3)" $ assertFailure' $ parseString "True(1,2,3)" -- reserved keyword
  ]

sqBrackTest = testGroup "Square Bracket Tests"
  [ testCase "[1,2,3]" $ parseString "[1,2,3]" @?= Right [SExp (List [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3)])]
  , testCase "[1,2,'3']" $ parseString "[1,2,'3']" @?= Right [SExp (List [Const (IntVal 1),Const (IntVal 2),Const (StringVal "3")])]
  , testCase "[1]" $ parseString "[1]" @?= Right [SExp (List [Const (IntVal 1)])]
  , testCase "[]" $ parseString "[]" @?= Right [SExp (List [])]
  , testCase "[1,True,False,None,'Hello',[], [1,2]]" $ parseString "[1,True,False,None,'Hello',[], [1,2]]" @?= Right [SExp (List [Const (IntVal 1),Const TrueVal,Const FalseVal,Const NoneVal,Const (StringVal "Hello"),List [],List [Const (IntVal 1),Const (IntVal 2)]])]
  ]

forClauseTest = testGroup "For Clause Test"
  [ testCase "[x for y in z if u > 2]" $ parseString "[x for y in z if u > 2]" @?= Right [SExp (Compr (Var "x") [CCFor "y" (Var "z"),CCIf (Oper Greater (Var "u") (Const (IntVal 2)))])]
  , testCase "[x\nfor\ty\tin\tz\tif\tu\t>\t2]" $ parseString "[x\nfor\ty\tin\tz\tif\tu\t>\t2]" @?= Right [SExp (Compr (Var "x") [CCFor "y" (Var "z"),CCIf (Oper Greater (Var "u") (Const (IntVal 2)))])]
  , testCase "[xfor y in z if u > 2]" $ assertFailure' $ parseString "[xfor y in z if u > 2]"
  , testCase "[x fory in z if u > 2]" $ assertFailure' $ parseString "[x fory in z if u > 2]"
  , testCase "[x for yin z if u > 2]" $ assertFailure' $ parseString "[x for yin z if u > 2]"
  , testCase "[x for y inz if u > 2]" $ assertFailure' $ parseString "[x for y inz if u > 2]"
  , testCase "[x for y in zif u > 2]" $ assertFailure' $ parseString "[x for y in zif u > 2]"
  , testCase "[x for y in z ifu > 2]" $ assertFailure' $ parseString "[x for y in z ifu > 2]"    
  , testCase "[(x)for(y)in(z)if(u)]" $ assertFailure' $ parseString "[(x)for(y)in(z)if(u)]"
  ]

commentsTest = testGroup "Comments test" 
  [ testCase "#Hello\tWorld!\nx" $ parseString "#Hello\tWorld!\nx" @?= Right [SExp (Var "x")]
  , testCase "#\nx" $ parseString "#\nx" @?= Right [SExp (Var "x")]
  , testCase "x #  \n" $ parseString "x #  \n" @?= Right [SExp (Var "x")]
  , testCase "#  \n#  \n#  \nx#  \n#  \n#  \n" $ parseString "#  \n#  \n#  \nx#  \n#  \n#  \n" @?= Right [SExp (Var "x")]
  , testCase "#  \n#  \n#  \nx#  \n#  \n#hej  " $ parseString "#  \n#  \n#  \nx#  \n#  \n#hej" @?= Right [SExp (Var "x")]
  ]

minimalTests = testGroup "Minimal tests" [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
      
  testCase "Big program" $
    parseString "x = [y*2 for y in range(1,10,2) if y > 5] # Some comment\n" @?=
      Right [SDef "x" (Compr (Oper Times (Var "y") (Const (IntVal 2))) [CCFor "y" (Call "range" [Const (IntVal 1),Const (IntVal 10),Const (IntVal 2)]),CCIf (Oper Greater (Var "y") (Const (IntVal 5)))])],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]
