-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

e = initEnv

tests :: TestTree
tests = testGroup "Tests" [showExpTests, evalSimpleTests, evalFullIfTests, evallFullLetTests, evalFullSumTest]

showExpTests = testGroup "showExp tests"
  [ testCase "showExp (Mul (Cst 2) (Add (Cst 3) (Cst 4))) should show (2*(3+4))" $ showExp (Mul (Cst 2) (Add (Cst 3) (Cst 4))) `compare` "(2*(3+4))" @?= EQ
  , testCase "showExp (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) should show ((2*3)+4)" $ showExp (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) `compare` "((2*3)+4)" @?= EQ
  , testCase "showExp (Exp (Cst 2) (Cst 3)) should show (2^3)" $ showExp (Pow (Cst 2) (Cst 3)) `compare` "(2^3)" @?= EQ
  -- , testCase "showExp  should show (2*3)^(2+3)+3" $
  --     showExp  `compare` "(2*3)^(2+3)" @?= EQ
  ]

evalSimpleTests = testGroup "evalSimple tests"
  [ testCase "2*(3+4) = 14" $ evalSimple (Mul (Cst 2) (Add (Cst 3) (Cst 4))) `compare` 14 @?= EQ
  , testCase "(2*3)+4 = 10" $ evalSimple (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) `compare` 10 @?= EQ
  -- , testCase "(2*3)+4 = 10" $ evalSimple (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) `compare` 10 @?= EQ
  ]

-- 0 = True
-- {...-1,1...} = True
evalFullIfTests = testGroup "evalFull If tests"
  [ testCase "If {test=Cst 1, yes=Cst 2, no=Cst 3} = 2" $ evalFull (If {test=Cst 1, yes=Cst 2, no=Cst 3}) e `compare` 2 @?= EQ
  , testCase "If {test=Cst 0, yes=Cst 2, no=Cst 3} = 3" $ evalFull (If {test=Cst 0, yes=Cst 2, no=Cst 3}) e `compare` 3 @?= EQ
  , testCase "If {test = Sub (Cst 2) (Cst 2), yes = Div (Cst 3) (Cst 0), no = Cst 5} doesnt throw error" $ evalFull (If {test = Sub (Cst 2) (Cst 2), yes = Div (Cst 3) (Cst 0), no = Cst 5}) initEnv `compare` 5 @?= EQ
  ]

evallFullLetTests = testGroup "evalFull Let tests"
  [ testCase "Let {var = \"x\", def = Add (Cst 3) (Cst 4),body = Mul (Var \"x\") (Var \"x\")} = 14" $ evalFull (Let {var = "x", def = Add (Cst 3) (Cst 4),body = Mul (Var "x") (Var "x")}) e `compare` 14 @?= EQ
  , testCase "Update equal value here" $ evalFull (Let {var = "x", def = Cst 5, body = Add (Let {var = "x", def = Add (Cst 3) (Cst 4), body = Mul (Var "x") (Var "x")}) (Var "x")}) e `compare` 14 @?= EQ
  , testCase "Should not throw error" $ evalFull (Let "x"  (Div (Cst 4) (Cst 0))  (Cst 5)) e `compare` 14 @?= EQ
  ]

evalFullSumTest = testGroup "evalFull Sum tests"
 [ testCase "Sum \"x\"  (Cst 1)  (Add (Cst 2) (Cst 2)) (Mul (Var \"x\") (Var \"x\")) = 30" $ evalFull (Sum "x"  (Cst 1)  (Add (Cst 2) (Cst 2)) (Mul (Var "x") (Var "x"))) e `compare` 30 @?= EQ
 ]
