-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [showExpTests, evalSimpleTests, extendEnvTests, evalFullTests, evalErrTests]

showExpTests = testGroup "showExp tests"
  [ testCase "Mul (Cst 2) (Add (Cst 3) (Cst 4))" $ showExp (Mul (Cst 2) (Add (Cst 3) (Cst 4))) `compare` "(2*(3+4))" @?= EQ
  , testCase "Add (Mul (Cst 2) (Cst 3)) (Cst 4)" $ showExp (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) `compare` "((2*3)+4)" @?= EQ
  , testCase "Exp (Cst 2) (Cst 3)" $ showExp (Pow (Cst 2) (Cst 3)) `compare` "(2^3)" @?= EQ
  , testCase "Add (Sub (Cst 2) (Cst 1)) (Cst 4)" $ showExp (Add (Sub (Cst 2) (Cst 1)) (Cst 4)) `compare` "((2-1)+4)" @?= EQ
  , testCase "Sub (Cst 1) (Add (Cst 3) (Cst 4))" $ showExp (Sub (Cst 1) (Add (Cst 3) (Cst 4))) `compare` "(1-(3+4))" @?= EQ
  , testCase "Div (Mul (Cst 2) (Cst 3)) (Cst 2)" $ showExp (Div (Mul (Cst 2) (Cst 3)) (Cst 2)) `compare` "((2*3)`div`2)" @?= EQ
  , testCase "Mul (Cst 5) (Div (Cst 3) (Cst 4))" $ showExp (Mul (Cst 5) (Div (Cst 3) (Cst 4))) `compare` "(5*(3`div`4))" @?= EQ
  , testCase "Pow (Cst 2) (Pow (Cst 3) (Cst 4))" $ showExp (Pow (Cst 2) (Pow (Cst 3) (Cst 4))) `compare` "(2^(3^4))" @?= EQ
  , testCase "Pow (Pow (Cst 2) (Cst 3)) (Cst 4)" $ showExp (Pow (Pow (Cst 2) (Cst 3)) (Cst 4)) `compare` "((2^3)^4)" @?= EQ
  , testCase "Cst 0" $ showExp (Cst 0) `compare` "0" @?= EQ
  , testCase "Cst (-5)" $ showExp (Cst (-5)) `compare` "(-5)" @?= EQ
  , testCase "Add (Cst (-10)) (Cst (-2))" $ showExp (Add (Cst (-10)) (Cst (-2))) `compare` "((-10)+(-2))" @?= EQ
  , testCase "Mul (Cst (-10)) (Cst (-2))" $ showExp (Mul (Cst (-10)) (Cst (-2))) `compare` "((-10)*(-2))" @?= EQ
  , testCase "Pow (Cst (-10)) (Cst (-2))" $ showExp (Pow (Cst (-10)) (Cst (-2))) `compare` "((-10)^(-2))" @?= EQ
  , testCase "Div (Cst (-10)) (Cst (-2))" $ showExp (Div (Cst (-10)) (Cst (-2))) `compare` "((-10)`div`(-2))" @?= EQ
  , testCase "Sub (Cst (-10)) (Cst (-2))" $ showExp (Sub (Cst (-10)) (Cst (-2))) `compare` "((-10)-(-2))" @?= EQ
  ]

evalSimpleTests = testGroup "evalSimple tests"
  [ testCase "Mul (Cst 2) (Add (Cst 3) (Cst 4))" $ evalSimple (Mul (Cst 2) (Add (Cst 3) (Cst 4))) `compare` 14 @?= EQ
  , testCase "Add (Mul (Cst 2) (Cst 3)) (Cst 4)" $ evalSimple (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) `compare` 10 @?= EQ
  , testCase "Cst 5" $ evalSimple (Cst 5) `compare` 5 @?= EQ
  , testCase "Cst (-5)" $ evalSimple (Cst (-5)) `compare` (-5) @?= EQ
  , testCase "Cst 789456123789456123" $ evalSimple (Cst 789456123789456123) `compare` 789456123789456123 @?= EQ
  , testCase "Cst (-789456123789456123)" $ evalSimple (Cst (-789456123789456123)) `compare` (-789456123789456123) @?= EQ
  , testCase "Add (Cst 1) (Cst 3)" $ evalSimple (Add (Cst 1) (Cst 3)) `compare` 4 @?= EQ
  , testCase "Sub (Cst 1) (Cst 3)" $ evalSimple (Sub (Cst 1) (Cst 3)) `compare` (-2) @?= EQ
  , testCase "Mul (Cst 2) (Cst 3)" $ evalSimple (Mul (Cst 2) (Cst 3)) `compare` 6 @?= EQ
  , testCase "Mul (Cst 789456123) (Cst 1234567890)" $ evalSimple (Mul (Cst 789456123) (Cst 1234567890)) `compare` 974637180019690470 @?= EQ
  , testCase "Div (Cst 10) (Cst 5)" $ evalSimple (Div (Cst 10) (Cst 5)) `compare` 2 @?= EQ
  , testCase "Div (Cst (-10)) (Cst 5)" $ evalSimple (Div (Cst (-10)) (Cst 5)) `compare` (-2) @?= EQ
  , testCase "Div (Cst 10) (Cst (-5))" $ evalSimple (Div (Cst 10) (Cst (-5))) `compare` (-2) @?= EQ
  , testCase "Div (Cst (-10)) (Cst (-5))" $ evalSimple (Div (Cst (-10)) (Cst (-5))) `compare` 2 @?= EQ
  , testCase "Div (Cst 11) (Cst 3)" $ evalSimple (Div (Cst 11) (Cst 3)) `compare` 3 @?= EQ
  , testCase "Div (Cst (-11)) (Cst 3)" $ evalSimple (Div (Cst (-11)) (Cst 3)) `compare` (-4) @?= EQ
  , testCase "Div (Cst 11) (Cst (-3))" $ evalSimple (Div (Cst 11) (Cst (-3))) `compare` (-4) @?= EQ
  , testCase "Div (Cst (-11)) (Cst (-3))" $ evalSimple (Div (Cst (-11)) (Cst (-3))) `compare` 3 @?= EQ
  , testCase "Div (Cst 0) (Cst 3)" $ evalSimple (Div (Cst 0) (Cst 3)) `compare` 0 @?= EQ
  , testCase "Pow (Cst 5) (Cst 5)" $ evalSimple (Pow (Cst 5) (Cst 5)) `compare` 3125 @?= EQ
  , testCase "Pow (Cst (-5)) (Cst 5)" $ evalSimple (Pow (Cst (-5)) (Cst 5)) `compare` (-3125) @?= EQ
  , testCase "Pow (Cst (-5)) (Cst 5)" $ evalSimple (Pow (Cst (-5)) (Cst 5)) `compare` (-3125) @?= EQ
  , testCase "Pow (Cst 0) (Cst 5)" $ evalSimple (Pow (Cst 0) (Cst 5)) `compare` 0 @?= EQ
  , testCase "Pow (Cst 0) (Cst 0)" $ evalSimple (Pow (Cst 0) (Cst 0)) `compare` 1 @?= EQ
  , testCase "Pow (Cst 5) (Cst 0)" $ evalSimple (Pow (Cst 5) (Cst 0)) `compare` 1 @?= EQ
  ]

extendEnvTests = testGroup "extendEnv tests"
  [ testCase "(extendEnv \"x\" 8 initEnv) \"x\"" $ extendEnv "x" 8 initEnv "x" `compare` Just 8 @?= EQ
  , testCase "(extendEnv \"x\" 8 initEnv) \"y\"" $ extendEnv "x" 8 initEnv "y" `compare` Nothing @?= EQ
  , testCase "(extendEnv \"y\" 2 (extendEnv \"x\" 8 initEnv)) \"y\"" $ extendEnv "y" 2 (extendEnv "x" 8 initEnv) "y" `compare` Just 2 @?= EQ
  , testCase "(extendEnv \"y\" 2 (extendEnv \"x\" 8 initEnv)) \"x\"" $ extendEnv "y" 2 (extendEnv "x" 8 initEnv) "x" `compare` Just 8 @?= EQ
  , testCase "(extendEnv \"y\" 2 (extendEnv \"x\" 8 initEnv)) \"z\"" $ extendEnv "y" 2 (extendEnv "x" 8 initEnv) "z" `compare` Nothing @?= EQ
  , testCase "(extendEnv \"x\" 2 (extendEnv \"x\" 8 initEnv)) \"x\"" $ extendEnv "x" 2 (extendEnv "x" 8 initEnv) "x" `compare` Just 2 @?= EQ
  ]

e = initEnv
e1 = extendEnv "y" 2 (extendEnv "x" 8 initEnv)

evalFullTests = testGroup "evalFull tests with [x := 2, y := 8]"
  [ testCase "Mul (Cst 2) (Add (Cst 3) (Cst 4))" $ evalFull (Mul (Cst 2) (Add (Cst 3) (Cst 4))) e1 `compare` 14 @?= EQ
  , testCase "Mul (Var \"x\") (Let \"x\" (Cst 5) (Var \"x\"))" $ evalFull (Mul (Var "x") (Let "x" (Cst 10) (Var "x"))) e1 `compare` 80 @?= EQ
  , testCase "Mul (Let \"x\" (Cst 5) (Var \"x\")) (Var \"x\")" $ evalFull (Mul (Let "x" (Cst 10) (Var "x")) (Var "x")) e1 `compare` 80 @?= EQ
  , testCase "Add (Mul (Cst 2) (Cst 3)) (Cst 4)" $ evalFull (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) e1 `compare` 10 @?= EQ
  , testCase "Cst 5" $ evalFull (Cst 5) e1 `compare` 5 @?= EQ
  , testCase "Cst (-5)" $ evalFull (Cst (-5)) e1 `compare` (-5) @?= EQ
  , testCase "Cst 789456123789456123" $ evalFull (Cst 789456123789456123) e1 `compare` 789456123789456123 @?= EQ
  , testCase "Cst (-789456123789456123)" $ evalFull (Cst (-789456123789456123)) e1 `compare` (-789456123789456123) @?= EQ
  , testCase "Add (Cst 1) (Cst 3)" $ evalFull (Add (Cst 1) (Cst 3)) e1 `compare` 4 @?= EQ
  , testCase "Sub (Cst 1) (Cst 3)" $ evalFull (Sub (Cst 1) (Cst 3)) e1 `compare` (-2) @?= EQ
  , testCase "Mul (Cst 2) (Cst 3)" $ evalFull (Mul (Cst 2) (Cst 3)) e1 `compare` 6 @?= EQ
  , testCase "Mul (Cst 789456123) (Cst 1234567890)" $ evalFull (Mul (Cst 789456123) (Cst 1234567890)) e1 `compare` 974637180019690470 @?= EQ
  , testCase "Div (Cst 10) (Cst 5)" $ evalFull (Div (Cst 10) (Cst 5)) e1 `compare` 2 @?= EQ
  , testCase "Div (Cst (-10)) (Cst 5)" $ evalFull (Div (Cst (-10)) (Cst 5)) e1 `compare` (-2) @?= EQ
  , testCase "Div (Cst 10) (Cst (-5))" $ evalFull (Div (Cst 10) (Cst (-5))) e1 `compare` (-2) @?= EQ
  , testCase "Div (Cst (-10)) (Cst (-5))" $ evalFull (Div (Cst (-10)) (Cst (-5))) e1 `compare` 2 @?= EQ
  , testCase "Div (Cst 11) (Cst 3)" $ evalFull (Div (Cst 11) (Cst 3)) e1 `compare` 3 @?= EQ
  , testCase "Div (Cst (-11)) (Cst 3)" $ evalFull (Div (Cst (-11)) (Cst 3)) e1 `compare` (-4) @?= EQ
  , testCase "Div (Cst 11) (Cst (-3))" $ evalFull (Div (Cst 11) (Cst (-3))) e1 `compare` (-4) @?= EQ
  , testCase "Div (Cst (-11)) (Cst (-3))" $ evalFull (Div (Cst (-11)) (Cst (-3))) e1 `compare` 3 @?= EQ
  , testCase "Div (Cst 0) (Cst 3)" $ evalFull (Div (Cst 0) (Cst 3)) e1 `compare` 0 @?= EQ
  , testCase "Pow (Cst 5) (Cst 5)" $ evalFull (Pow (Cst 5) (Cst 5)) e1 `compare` 3125 @?= EQ
  , testCase "Pow (Cst (-5)) (Cst 5)" $ evalFull (Pow (Cst (-5)) (Cst 5)) e1 `compare` (-3125) @?= EQ
  , testCase "Pow (Cst (-5)) (Cst 5)" $ evalFull (Pow (Cst (-5)) (Cst 5)) e1 `compare` (-3125) @?= EQ
  , testCase "Pow (Cst 0) (Cst 5)" $ evalFull (Pow (Cst 0) (Cst 5)) e1 `compare` 0 @?= EQ
  , testCase "Pow (Cst 0) (Cst 0)" $ evalFull (Pow (Cst 0) (Cst 0)) e1 `compare` 1 @?= EQ
  , testCase "Pow (Cst 5) (Cst 0)" $ evalFull (Pow (Cst 5) (Cst 0)) e1 `compare` 1 @?= EQ
  , testCase "If {test=Cst 1, yes=Cst 2, no=Cst 3}" $ evalFull (If {test=Cst 1, yes=Cst 2, no=Cst 3}) e1 `compare` 2 @?= EQ
  , testCase "If {test=Cst 1, yes=Sub (Cst 1) (Cst 1), no=Cst 3}" $ evalFull (If {test=Cst 1, yes=Sub (Cst 1) (Cst 1), no=Cst 3}) e1 `compare` 0 @?= EQ
  , testCase "If {test=Cst 1, yes=Cst 2, no=Sub (Cst 1) (Cst 1)}" $ evalFull (If {test=Cst 1, yes=Cst 2, no=Sub (Cst 1) (Cst 1)}) e1 `compare` 2 @?= EQ
  , testCase "If {test=Cst 0, yes=Cst 2, no=Sub (Cst 1) (Cst 1)}" $ evalFull (If {test=Cst 0, yes=Cst 2, no=Sub (Cst 1) (Cst 1)}) e1 `compare` 0 @?= EQ
  , testCase "If {test=Cst 1, yes=Sub (Cst 1) (Cst 1), no=Cst 2}" $ evalFull (If {test=Cst 1, yes=Sub (Cst 1) (Cst 1), no=Cst 2}) e1 `compare` 0 @?= EQ
  , testCase "If {test=Cst (-1), yes=Cst 2, no=Cst 3}" $ evalFull (If {test=Cst 1, yes=Cst 2, no=Cst 3}) e1 `compare` 2 @?= EQ
  , testCase "If {test=(Sub (Cst 1) (Cst 1)), yes=Cst 2, no=Cst 3}" $ evalFull (If {test=Cst 1, yes=Cst 2, no=Cst 3}) e1 `compare` 2 @?= EQ
  , testCase "If {test=Cst 0, yes=Cst 2, no=Cst 3}" $ evalFull (If {test=Cst 0, yes=Cst 2, no=Cst 3}) e1 `compare` 3 @?= EQ
  , testCase "If {test = Sub (Cst 2) (Cst 2), yes = Div (Cst 3) (Cst 0), no = Cst 5}" $ evalFull (If {test = Sub (Cst 2) (Cst 2), yes = Div (Cst 3) (Cst 0), no = Cst 5}) e1 `compare` 5 @?= EQ
  , testCase "Var \"x\"" $ evalFull (Var "x") e1 `compare` 8 @?= EQ
  , testCase "Var \"y\"" $ evalFull (Var "y") e1 `compare` 2 @?= EQ
  , testCase "Let {var = \"x\", def = Add (Cst 3) (Cst 4),body = Mul (Var \"x\") (Var \"x\")}" $ evalFull (Let {var = "x", def = Add (Cst 3) (Cst 4),body = Mul (Var "x") (Var "x")}) e1 `compare` 49 @?= EQ
  , testCase "Let \"x\"  (Div (Cst 4) (Cst 0))  (Cst 5)" $ evalFull (Let "x"  (Div (Cst 4) (Cst 0))  (Cst 5)) e1 `compare` 5 @?= EQ
  , testCase "Let \"z\" (Add (Cst 5) (Cst 2)) (Var \"z\")" $ evalFull (Let "z" (Add (Cst 5) (Cst 2)) (Var "z")) e1 `compare` 7 @?= EQ
  , testCase "Let \"z\" (Add (Cst 5) (Cst 2)) (Var \"z\")" $ evalFull (Let "z" (Add (Cst 5) (Cst 2)) (Var "z")) e1 `compare` 7 @?= EQ
  , testCase "Let \"z\" (Add (Cst 5) (Cst 2)) (Pow (Var \"z\") (Var \"z\"))" $ evalFull (Let "z" (Add (Cst 5) (Cst 2)) (Pow (Var "z") (Var "z"))) e1 `compare` 823543 @?= EQ
  , testCase "Let \"x\" (Add (Cst 4) (Var \"x\")) (Var \"x\")" $ evalFull (Let "x" (Add (Cst 4) (Var "x")) (Var "x")) e1 `compare` 12 @?= EQ
  , testCase "Let \"x\" (Add (Cst 4) (Var \"y\")) (Var \"x\")" $ evalFull (Let "x" (Add (Cst 4) (Var "y")) (Var "x")) e1 `compare` 6 @?= EQ
  , testCase "Let \"x\" (Add (Cst 4) (Var \"y\")) (Var \"y\")" $ evalFull (Let "x" (Add (Cst 4) (Var "y")) (Var "y")) e1 `compare` 2 @?= EQ
  , testCase "Let \"x\" (Add (Cst 5) (Var \"y\")) (Let \"y\" (Mul (Var \"x\") (Cst 8)) (Var \"x\"))" $ evalFull (Let "x" (Add (Cst 5) (Var "y")) (Let "y" (Mul (Var "x") (Cst 8)) (Var "x"))) e1 `compare` 7 @?= EQ
  , testCase "Let \"x\" (Add (Cst 5) (Var \"y\")) (Let \"y\" (Mul (Var \"x\") (Cst 8)) (Var \"y\"))" $ evalFull (Let "x" (Add (Cst 5) (Var "y")) (Let "y" (Mul (Var "x") (Cst 8)) (Var "y"))) e1 `compare` 56 @?= EQ
  , testCase "Let \"x\" (Let \"y\" (Cst 5) (Sub (Var \"x\") (Var \"y\"))) (Mul (Var \"x\") (Var \"y\"))" $ evalFull (Let "x" (Let "y" (Cst 5) (Sub (Var "x") (Var "y"))) (Mul (Var "x") (Var "y"))) e1 `compare` 6 @?= EQ
  , testCase "Let \"a\" (Var \"x\") (Let \"x\" (Cst 8) (Var \"a\"))" $ evalFull (Let "a" (Var "x") (Let "x" (Cst 8) (Var "a"))) e1 `compare` 8 @?= EQ
  , testCase "Sum \"x\" (Sub (Cst 2) (Cst 5)) (Add (Cst 8) (Cst 5)) (Var \"x\")" $ evalFull (Sum "x" (Sub (Cst 2) (Cst 5)) (Add (Cst 8) (Cst 5)) (Var "x")) e1 `compare` 85 @?= EQ
  , testCase "Sum \"x\" (Cst 2) (Cst 10) (Pow (Var \"x\") (Cst 8))" $ evalFull (Sum "x" (Cst 2) (Cst 10) (Pow (Var "x") (Cst 8))) e1 `compare` 167731332 @?= EQ
  , testCase "Sum \"x\" (Cst 10) (Add (Cst 5) (Cst 5)) (Mul (Cst 7) (Var \"x\"))" $ evalFull (Sum "x" (Cst 10) (Add (Cst 5) (Cst 5)) (Mul (Cst 7) (Var "x"))) e1 `compare` 70 @?= EQ
  , testCase "Sum \"x\" (Cst 11) (Add (Cst 5) (Cst 5)) (Var \"x\")" $ evalFull (Sum "x" (Cst 11) (Add (Cst 5) (Cst 5)) (Var "x")) e1 `compare` 0 @?= EQ
  , testCase "Sum \"x\" (Cst 12) (Add (Cst 5) (Cst 5)) (Div (Var \"x\") (Cst 0))" $ evalFull (Sum "x" (Cst 12) (Add (Cst 5) (Cst 5)) (Div (Var "x") (Cst 0))) e1 `compare` 0 @?= EQ
  , testCase "Sum \"x\" (Cst 123456789012345) (Cst 0) (Cst 1)" $ evalFull (Sum "x" (Cst 123456789012345) (Cst 0) (Cst 1)) e1 `compare` 0 @?= EQ
  , testCase "Sum \"x\" (Cst 3) (Var \"x\") (Let \"x\" (Add (Var \"x\") (Cst 1)) (Var \"x\"))" $ evalFull (Sum "x" (Cst 3) (Var "x") (Let "x" (Add (Var "x") (Cst 1)) (Var "x"))) e1 `compare` 39 @?= EQ
  , testCase "Sum \"x\" (Cst 3) (Var \"x\") (Sum \"x\" (Var \"x\") (Cst 10) (Var \"x\"))" $ evalFull (Sum "x" (Cst 3) (Var "x") (Sum "x" (Var "x") (Cst 10) (Var "x"))) e1 `compare` 247 @?= EQ
  , testCase "Sum \"x\"  (Cst 1)  (Add (Cst 2) (Cst 2)) (Mul (Var \"x\") (Var \"x\")) = 30" $ evalFull (Sum "x"  (Cst 1)  (Add (Cst 2) (Cst 2)) (Mul (Var "x") (Var "x"))) e1 `compare` 30 @?= EQ
 ]


