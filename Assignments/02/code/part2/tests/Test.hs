-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Tests" [operateTest, stringifyValuesTest, lookTest]

operateTest = testGroup "operate tests" 
  [ testCase "operate Plus (IntVal 2) (IntVal 2)" $ operate Plus (IntVal 2) (IntVal 2) @?= Right (IntVal 4)
  , testCase "operate Plus (IntVal 2) (IntVal (-2))" $ operate Plus (IntVal 2) (IntVal (-2)) @?= Right (IntVal 0)
  , testCase "operate Plus (IntVal (-2)) (IntVal (-2))" $ operate Plus (IntVal (-2)) (IntVal (-2)) @?= Right (IntVal (-4))
  , testCase "operate Plus (IntVal (-2)) (IntVal 2)" $ operate Plus (IntVal (-2)) (IntVal 2) @?= Right (IntVal 0)
  , testCase "*operate Plus TrueVal (IntVal 2)" $ operate Plus TrueVal (IntVal 2) @?= Left "Only integers allowed for Plus Op"
  , testCase "*operate Plus FalseVal (IntVal 2)" $ operate Plus FalseVal (IntVal 2) @?= Left "Only integers allowed for Plus Op"
  , testCase "*operate Plus (StringVal \"Hello\") (IntVal 2)" $ operate Plus (StringVal "Hello") (IntVal 2) @?= Left "Only integers allowed for Plus Op"
  ]

lookTest = testGroup "lookTest" 
  [testCase "*look \"x\"" $ 
    do x <- look "x" 
       x @?= Left (EBadVar "x")
  ]  

stringifyValuesTest = testGroup "stringifyValues Tests" 
  [ testCase "stringifyValues [TrueVal, TrueVal, TrueVal]" $ stringifyValues [TrueVal, TrueVal, TrueVal] @?= "True True True"
  , testCase "stringifyValues [IntVal 42, StringVal \"foo\", ListVal [TrueVal, ListVal []], IntVal (-1)]" $ stringifyValues [IntVal 42, StringVal "foo", ListVal [TrueVal, ListVal []], IntVal (-1)] @?= "42 foo [True, []] -1"

  ]

stubbyTest = testGroup "Stubby tests" 
  [testCase "crash test" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                           (Const (IntVal 2))]),
             SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello"))
  , testCase "execute misc.ast from handout" $
     do pgm <- read <$> readFile "examples/misc.ast"
        out <- readFile "examples/misc.out"
        execute pgm @?= (lines out, Nothing)]
