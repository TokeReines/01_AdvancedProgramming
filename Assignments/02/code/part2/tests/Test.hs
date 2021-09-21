-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit
import BoaAST (Value(NoneVal))
import Data.Bool (Bool(True))

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Tests" [lookTest, abortTest, truthyTest, operateTest, stringifyValuesTest, applyTest, stubbyTest]

lookTest = testGroup "look Tests" 
  [ testCase "*look \"x\"" $ runComp (look "x") [] @?= (Left (EBadVar "x"),[])
  , testCase "look \"x\" with [(\"x\", NoneVal)]" $ runComp (look "x") [("x", NoneVal)] @?= (Right NoneVal,[])
  , testCase "look \"x\" with [(\"x\", FalseVal)]" $ runComp (look "x") [("x", FalseVal)] @?= (Right FalseVal,[])
  , testCase "look \"x\" with [(\"x\", IntVal 5)]" $ runComp (look "x") [("x", IntVal 5)] @?= (Right (IntVal 5),[])
  , testCase "look \"x\" with [(\"x\", (StringVal \"Hello World!\"))]" $ runComp (look "x") [("x", StringVal "Hello World!")] @?= (Right (StringVal "Hello World!"),[])
  , testCase "look \"x\" with [(\"x\", ListVal [TrueVal, FalseVal])]" $ runComp (look "x") [("x", ListVal [TrueVal, FalseVal])] @?= (Right (ListVal [TrueVal, FalseVal]),[])
  , testCase "look \"y\" with [(\"x\", NoneVal), (\"y\", TrueVal)]" $ runComp (look "y") [("x", NoneVal), ("y", TrueVal)] @?= (Right TrueVal,[])
  , testCase "*look \"z\" with [(\"x\", NoneVal), (\"y\", TrueVal)]" $ runComp (look "z") [("x", NoneVal), ("y", TrueVal)] @?= (Left (EBadVar "z"),[])
  , testCase "look \"y\" with [(\"y\", NoneVal), (\"x\", TrueVal)]" $ runComp (look "y") [("y", NoneVal), ("x", TrueVal)] @?= (Right NoneVal,[])
  , testCase "look \"x\" with [(\"x\", NoneVal), (\"x\", TrueVal)]" $ runComp (look "x") [("x", NoneVal), ("x", TrueVal)] @?= (Right NoneVal,[])
  , testCase "look \"squares\" with [(\"x\", NoneVal), (\"squares\", TrueVal)]" $ runComp (look "squares") [("x", NoneVal), ("squares", TrueVal)] @?= (Right TrueVal,[])
  ]

abortTest = testGroup "abort tests"
  [ testCase "abort (EBadVar \"x\")" $ (runComp (abort (EBadVar "x")) [] :: (Either RunError Int, [String])) @?= (Left (EBadVar "x"),[])
  , testCase "abort (EBadFun \"x\")" $ (runComp (abort (EBadFun "x")) [] :: (Either RunError Int, [String])) @?= (Left (EBadFun "x"),[])
  , testCase "abort (EBadArg \"x\")" $ (runComp (abort (EBadArg "x")) [] :: (Either RunError Int, [String])) @?= (Left (EBadArg "x"),[])
  ]

withBindingTest = testGroup "withBinding tests" []

outputTest = testGroup "output tests" []


truthyTest = testGroup "truthy tests" 
 [ testCase "truthy NoneVal" $ truthy NoneVal @?= False
 , testCase "truthy FalseVal" $ truthy FalseVal @?= False
 , testCase "truthy TrueVal" $ truthy TrueVal @?= True
 , testCase "truthy (IntVal 0)" $ truthy (IntVal 0) @?= False
 , testCase "truthy (IntVal 1)" $ truthy (IntVal 1) @?= True
 , testCase "truthy (IntVal (-1))" $ truthy (IntVal (-1)) @?= True
 , testCase "truthy (StringVal \"\")" $ truthy (StringVal "") @?= False
 , testCase "truthy (StringVal \"Hello World!\")" $ truthy (StringVal "Hello World!") @?= True
 , testCase "truthy (ListVal [])" $ truthy (ListVal []) @?= False
 , testCase "truthy (ListVal [TrueVal, FalseVal])" $ truthy (ListVal [TrueVal, FalseVal]) @?= True

 ]

operateTest = testGroup "operate tests" 
  [ testCase "operate Plus (IntVal 2) (IntVal 2)" $ operate Plus (IntVal 2) (IntVal 2) @?= Right (IntVal 4)
  , testCase "operate Plus (IntVal 2) (IntVal (-2))" $ operate Plus (IntVal 2) (IntVal (-2)) @?= Right (IntVal 0)
  , testCase "operate Plus (IntVal (-2)) (IntVal (-2))" $ operate Plus (IntVal (-2)) (IntVal (-2)) @?= Right (IntVal (-4))
  , testCase "operate Plus (IntVal (-2)) (IntVal 2)" $ operate Plus (IntVal (-2)) (IntVal 2) @?= Right (IntVal 0)
  , testCase "*operate Plus TrueVal (IntVal 2)" $ operate Plus TrueVal (IntVal 2) @?= Left "Only integers allowed for Plus Op"
  , testCase "*operate Plus FalseVal (IntVal 2)" $ operate Plus FalseVal (IntVal 2) @?= Left "Only integers allowed for Plus Op"
  , testCase "*operate Plus (StringVal \"Hello\") (IntVal 2)" $ operate Plus (StringVal "Hello") (IntVal 2) @?= Left "Only integers allowed for Plus Op"
  ]

applyTest = testGroup "apply tests" 
  [ testCase "apply \"range\" [IntVal 3]" $ runComp (apply "range" [IntVal 3]) [] @?= (Right (ListVal [IntVal 0,IntVal 1,IntVal 2]),[])
  , testCase "apply \"range\" [Intval 0]" $ runComp (apply "range" [IntVal 0]) [] @?= (Right (ListVal []),[])
  , testCase "apply \"range\" [Intval (-2)]" $ runComp (apply "range" [IntVal (-2)]) [] @?= (Right (ListVal []),[])
  , testCase "*apply \"range\" [NoneVal]" $ runComp (apply "range" [NoneVal]) [] @?= (Left (EBadArg "Only integer values allowed as augments for function \"range\""),[])
  , testCase "*apply \"range\" [TrueVal]" $ runComp (apply "range" [TrueVal]) [] @?= (Left (EBadArg "Only integer values allowed as augments for function \"range\""),[])
  , testCase "*apply \"range\" [FalseVal]" $ runComp (apply "range" [FalseVal]) [] @?= (Left (EBadArg "Only integer values allowed as augments for function \"range\""),[])
  , testCase "*apply \"range\" [StringVal \"Hellow World!\"]" $ runComp (apply "range" [StringVal "Hellow World!"]) [] @?= (Left (EBadArg "Only integer values allowed as augments for function \"range\""),[])
  , testCase "*apply \"range\" [ListVal []]" $ runComp (apply "range" [ListVal []]) [] @?= (Left (EBadArg "Only integer values allowed as augments for function \"range\""),[])
  , testCase "apply \"range\" [IntVal 2, IntVal 4]" $ runComp (apply "range" [IntVal 2, IntVal 4]) [] @?= (Right (ListVal [IntVal 2,IntVal 3]),[])
  , testCase "apply \"range\" [IntVal 2, IntVal 2]" $ runComp (apply "range" [IntVal 2, IntVal 2]) [] @?= (Right (ListVal []),[])
  , testCase "apply \"range\" [IntVal 4, IntVal 2]" $ runComp (apply "range" [IntVal 4, IntVal 2]) [] @?= (Right (ListVal []),[])
  , testCase "*apply \"range\" [IntVal 4, TrueVal]" $ runComp (apply "range" [IntVal 4, TrueVal]) [] @?= (Left (EBadArg "Only integer values allowed as augments for function \"range\""),[])
  , testCase "apply \"range\" [IntVal 0, IntVal 6, IntVal 2]" $ runComp (apply "range" [IntVal 0, IntVal 6, IntVal 2]) [] @?= (Right (ListVal [IntVal 0,IntVal 2, IntVal 4]),[])
  , testCase "apply \"range\" [IntVal 6, IntVal 0, IntVal (-2)]" $ runComp (apply "range" [IntVal 6, IntVal 0, IntVal (-2)]) [] @?= (Right (ListVal [IntVal 4,IntVal 2, IntVal 0]),[])
  , testCase "apply \"range\" [IntVal 3, IntVal 10, IntVal 2]" $ runComp (apply "range" [IntVal 3, IntVal 10, IntVal 2]) [] @?= (Right (ListVal [IntVal 3,IntVal 5, IntVal 7, IntVal 9]),[])
  , testCase "apply \"range\" [IntVal 3, IntVal 11, IntVal 2]" $ runComp (apply "range" [IntVal 3, IntVal 11, IntVal 2]) [] @?= (Right (ListVal [IntVal 3,IntVal 5, IntVal 7, IntVal 9]),[])
  , testCase "apply \"range\" [IntVal 0, IntVal 6, IntVal (-2)]" $ runComp (apply "range" [IntVal 0, IntVal 6, IntVal (-2)]) [] @?= (Right (ListVal []),[])
  , testCase "apply \"range\" [IntVal 6, IntVal 0, IntVal (2)]" $ runComp (apply "range" [IntVal 6, IntVal 0, IntVal 2]) [] @?= (Right (ListVal []),[])
  , testCase "apply \"range\" [IntVal 6, IntVal 0, IntVal (2)]" $ runComp (apply "range" [IntVal 0, IntVal 0, IntVal 2]) [] @?= (Right (ListVal []),[])
  , testCase "apply \"range\" [IntVal 6, IntVal 0, IntVal (2)]" $ runComp (apply "range" [IntVal 0, IntVal 0, IntVal (-2)]) [] @?= (Right (ListVal []),[])
  , testCase "*apply \"range\" [IntVal 6, IntVal 0, IntVal (0)]" $ runComp (apply "range" [IntVal 6, IntVal 0, IntVal 0]) [] @?= (Left (EBadArg "Stepsize may not be 0"),[])
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
  -- , testCase "execute misc.ast from handout" $
  --    do pgm <- read <$> readFile "examples/misc.ast"
  --       out <- readFile "examples/misc.out"
  --       execute pgm @?= (lines out, Nothing)
  ]
