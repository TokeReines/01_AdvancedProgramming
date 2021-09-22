-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Tests" [lookTest, abortTest, withBindingTest, outputTest, truthyTest, operateTest, applyTest, evalTest, stubbyTest]

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

withBindingTest = testGroup "withBinding tests" 
  [ testCase "withBinding \"x\" (IntVal 3) (eval (Var \"x\"))" $ runComp (withBinding "x" (IntVal 3) (eval (Var "x"))) [] @?= (Right (IntVal 3),[])
  , testCase "*withBinding \"x\" (IntVal 3) (eval (Var \"y\"))" $ runComp (withBinding "x" (IntVal 3) (eval (Var "y"))) [] @?= (Left (EBadVar "y"),[])
  , testCase "do withBinding \"x\" (IntVal 3) (eval (Var \"x\")); withBinding \"x\" (IntVal 4) (eval (Var \"x\"))" $ runComp (do withBinding "x" (IntVal 3) (eval (Var "x")); withBinding "x" (IntVal 4) (eval (Var "x"))) [] @?= (Right (IntVal 4),[])
  , testCase "*do withBinding \"y\" (IntVal 3) (eval (Var \"x\")); withBinding \"x\" (IntVal 4) (eval (Var \"x\"))" $ runComp (do withBinding "y" (IntVal 3) (eval (Var "x")); withBinding "x" (IntVal 4) (eval (Var "x"))) [] @?= (Left (EBadVar "x"),[])
  , testCase "do withBinding \"x\" (IntVal 3) (eval (Var \"x\")); withBinding \"y\" (IntVal 4) (eval (Var \"y\"))" $ runComp (do withBinding "x" (IntVal 3) (eval (Var "x")); withBinding "y" (IntVal 4) (eval (Var "y"))) [] @?= (Right (IntVal 4),[])
  ]

outputTest = testGroup "output tests" 
  [ testCase "output \"Hello\"" $ runComp (output "Hello") [] @?= (Right (),["Hello"])
  , testCase "do output \"Hello\"; output \"World\"" $ runComp (do output "Hello"; output "World") [] @?= (Right (),["Hello", "World"])
  ]

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
 , testCase "truthy (ListVal [FalseVal, FalseVal])" $ truthy (ListVal [FalseVal, FalseVal]) @?= True

 ]

operateTest = testGroup "operate tests" 
  [ -- Plus
    testCase "operate Plus (IntVal 2) (IntVal 2)" $ operate Plus (IntVal 2) (IntVal 2) @?= Right (IntVal 4)
  , testCase "operate Plus (IntVal 2147483648) (IntVal 2147483648)" $ operate Plus (IntVal 2147483648) (IntVal 2147483648) @?= Right (IntVal 4294967296)
  , testCase "operate Plus (IntVal 2) (IntVal (-2))" $ operate Plus (IntVal 2) (IntVal (-2)) @?= Right (IntVal 0)
  , testCase "operate Plus (IntVal (-2)) (IntVal (-2))" $ operate Plus (IntVal (-2)) (IntVal (-2)) @?= Right (IntVal (-4))
  , testCase "operate Plus (IntVal (-2)) (IntVal 2)" $ operate Plus (IntVal (-2)) (IntVal 2) @?= Right (IntVal 0)
  , testCase "*operate Plus TrueVal (IntVal 2)" $ operate Plus TrueVal (IntVal 2) @?= Left "Only integers allowed for Plus Op"
  , testCase "*operate Plus FalseVal (IntVal 2)" $ operate Plus FalseVal (IntVal 2) @?= Left "Only integers allowed for Plus Op"
  , testCase "*operate Plus (StringVal \"Hello\") (IntVal 2)" $ operate Plus (StringVal "Hello") (IntVal 2) @?= Left "Only integers allowed for Plus Op"
  , testCase "*operate Plus (ListVal []) (IntVal 2)" $ operate Plus (ListVal []) (IntVal 2) @?= Left "Only integers allowed for Plus Op"
    -- Minus
  , testCase "operate Minus (IntVal 2) (IntVal 2)" $ operate Minus (IntVal 2) (IntVal 2) @?= Right (IntVal 0)
  , testCase "operate Minus (IntVal -1) (IntVal 2147483648)" $ operate Minus (IntVal (-1)) (IntVal 2147483648) @?= Right (IntVal (-2147483649))
  , testCase "operate Minus (IntVal 2) (IntVal (-2))" $ operate Minus (IntVal 2) (IntVal (-2)) @?= Right (IntVal 4)
  , testCase "operate Minus (IntVal (-2)) (IntVal (-2))" $ operate Minus (IntVal (-2)) (IntVal (-2)) @?= Right (IntVal 0)
  , testCase "operate Minus (IntVal (-2)) (IntVal 2)" $ operate Minus (IntVal (-2)) (IntVal 2) @?= Right (IntVal (-4))
  , testCase "*operate Minus TrueVal (IntVal 2)" $ operate Minus TrueVal (IntVal 2) @?= Left "Only integers allowed for Minus Op"
  , testCase "*operate Minus FalseVal (IntVal 2)" $ operate Minus FalseVal (IntVal 2) @?= Left "Only integers allowed for Minus Op"
  , testCase "*operate Minus (StringVal \"Hello\") (IntVal 2)" $ operate Minus (StringVal "Hello") (IntVal 2) @?= Left "Only integers allowed for Minus Op"
  , testCase "*operate Minus (ListVal []) (IntVal 2)" $ operate Minus (ListVal []) (IntVal 2) @?= Left "Only integers allowed for Minus Op"
    -- Times
  , testCase "operate Times (IntVal 2) (IntVal 2)" $ operate Times (IntVal 2) (IntVal 2) @?= Right (IntVal 4)
  , testCase "operate Times (IntVal 1234567890) (IntVal 5)" $ operate Times (IntVal 1234567890) (IntVal 5) @?= Right (IntVal 6172839450)
  , testCase "operate Times (IntVal 2) (IntVal (-2))" $ operate Times (IntVal 2) (IntVal (-2)) @?= Right (IntVal (-4))
  , testCase "operate Times (IntVal (-2)) (IntVal (-2))" $ operate Times (IntVal (-2)) (IntVal (-2)) @?= Right (IntVal 4)
  , testCase "operate Times (IntVal (-2)) (IntVal 2)" $ operate Times (IntVal (-2)) (IntVal 2) @?= Right (IntVal (-4))
  , testCase "operate Times (IntVal (-2)) (IntVal 0)" $ operate Times (IntVal (-2)) (IntVal 0) @?= Right (IntVal 0)
  , testCase "operate Times (IntVal 0) (IntVal 2)" $ operate Times (IntVal 0) (IntVal 2) @?= Right (IntVal (0))
  , testCase "*operate Times TrueVal (IntVal 2)" $ operate Times TrueVal (IntVal 2) @?= Left "Only integers allowed for Times Op"
  , testCase "*operate Times FalseVal (IntVal 2)" $ operate Times FalseVal (IntVal 2) @?= Left "Only integers allowed for Times Op"
  , testCase "*operate Times (StringVal \"Hello\") (IntVal 2)" $ operate Times (StringVal "Hello") (IntVal 2) @?= Left "Only integers allowed for Times Op"
  , testCase "*operate Times (ListVal []) (IntVal 2)" $ operate Times (ListVal []) (IntVal 2) @?= Left "Only integers allowed for Times Op"
    -- Div
  , testCase "operate Div (IntVal 2) (IntVal 2)" $ operate Div (IntVal 9) (IntVal 2) @?= Right (IntVal 4)
  , testCase "operate Div (IntVal 2) (IntVal 2)" $ operate Div (IntVal 0) (IntVal 2) @?= Right (IntVal 0)
  , testCase "operate Div (IntVal 2) (IntVal 0)" $ operate Div (IntVal 9) (IntVal 0) @?= Left "Division by 0 not allowed"
  , testCase "operate Div (IntVal 2147483648) (IntVal 2147483648)" $ operate Div (IntVal 2147483648) (IntVal 2147483000) @?= Right (IntVal 1)
  , testCase "operate Div (IntVal 7) (IntVal (-2))" $ operate Div (IntVal 7) (IntVal (-2)) @?= Right (IntVal (-4))
  , testCase "operate Div (IntVal (-2)) (IntVal (-2))" $ operate Div (IntVal (-2)) (IntVal (-2)) @?= Right (IntVal 1)
  , testCase "operate Div (IntVal (-2)) (IntVal 2)" $ operate Div (IntVal (-2)) (IntVal 2) @?= Right (IntVal (-1))
  , testCase "*operate Div TrueVal (IntVal 2)" $ operate Div TrueVal (IntVal 2) @?= Left "Only integers allowed for Div Op"
  , testCase "*operate Div FalseVal (IntVal 2)" $ operate Div FalseVal (IntVal 2) @?= Left "Only integers allowed for Div Op"
  , testCase "*operate Div (StringVal \"Hello\") (IntVal 2)" $ operate Div (StringVal "Hello") (IntVal 2) @?= Left "Only integers allowed for Div Op"
  , testCase "*operate Div (ListVal []) (IntVal 2)" $ operate Div (ListVal []) (IntVal 2) @?= Left "Only integers allowed for Div Op"
    -- Mod
  , testCase "operate Mod (IntVal 2) (IntVal 2)" $ operate Mod (IntVal 9) (IntVal 2) @?= Right (IntVal 1)
  , testCase "operate Mod (IntVal 2) (IntVal 2)" $ operate Mod (IntVal 0) (IntVal 2) @?= Right (IntVal 0)
  , testCase "operate Mod (IntVal 2) (IntVal 0)" $ operate Mod (IntVal 9) (IntVal 0) @?= Left "Modulo by 0 not allowed"
  , testCase "operate Mod (IntVal 2147483648) (IntVal 2147483648)" $ operate Mod (IntVal 2147483648) (IntVal 2147483000) @?= Right (IntVal 648)
  , testCase "operate Mod (IntVal 7) (IntVal (-2))" $ operate Mod (IntVal 7) (IntVal (-2)) @?= Right (IntVal (-1))
  , testCase "operate Mod (IntVal (-2)) (IntVal (-2))" $ operate Mod (IntVal (-2)) (IntVal (-2)) @?= Right (IntVal 0)
  , testCase "operate Mod (IntVal (-2)) (IntVal 2)" $ operate Mod (IntVal (-2)) (IntVal 2) @?= Right (IntVal 0)
  , testCase "operate Mod (IntVal (-9)) (IntVal (-2))" $ operate Mod (IntVal (-9)) (IntVal (-2)) @?= Right (IntVal (-1))
  , testCase "operate Mod (IntVal (-9)) (IntVal 2)" $ operate Mod (IntVal (-9)) (IntVal 2) @?= Right (IntVal 1)
  , testCase "*operate Mod TrueVal (IntVal 2)" $ operate Mod TrueVal (IntVal 2) @?= Left "Only integers allowed for Mod Op"
  , testCase "*operate Mod FalseVal (IntVal 2)" $ operate Mod FalseVal (IntVal 2) @?= Left "Only integers allowed for Mod Op"
  , testCase "*operate Mod (StringVal \"Hello\") (IntVal 2)" $ operate Mod (StringVal "Hello") (IntVal 2) @?= Left "Only integers allowed for Mod Op"
  , testCase "*operate Mod (ListVal []) (IntVal 2)" $ operate Mod (ListVal []) (IntVal 2) @?= Left "Only integers allowed for Mod Op"
    -- Eq
  , testCase "operate Eq (IntVal 1) (IntVal 1)" $ operate Eq (IntVal 1) (IntVal 1) @?= Right TrueVal
  , testCase "operate Eq TrueVal TrueVal" $ operate Eq TrueVal TrueVal @?= Right TrueVal
  , testCase "operate Eq FalseVal FalseVal" $ operate Eq FalseVal FalseVal @?= Right TrueVal
  , testCase "operate Eq NoneVal NoneVal" $ operate Eq NoneVal NoneVal @?= Right TrueVal
  , testCase "operate Eq (StringVal \"Hello World\") (StringVal \"Hello World\")" $ operate Eq (StringVal "Hello World") (StringVal "Hello World") @?= Right TrueVal
  , testCase "operate Eq (ListVal []) (ListVal [])" $ operate Eq (ListVal []) (ListVal []) @?= Right TrueVal
  , testCase "operate Eq (ListVal [FalseVal, FalseVal]) (ListVal [FalseVal, FalseVal])" $ operate Eq (ListVal [FalseVal, FalseVal]) (ListVal [FalseVal, FalseVal]) @?= Right TrueVal
  , testCase "operate Eq (ListVal [IntVal 42, StringVal \"foo\", ListVal [TrueVal, ListVal []], IntVal (-1)]) (ListVal [IntVal 42, StringVal \"foo\", ListVal [TrueVal, ListVal []], IntVal (-1)])" $ operate Eq (ListVal [IntVal 42, StringVal "foo", ListVal [TrueVal, ListVal []], IntVal (-1)]) (ListVal [IntVal 42, StringVal "foo", ListVal [TrueVal, ListVal []], IntVal (-1)]) @?= Right TrueVal
  , testCase "operate Eq (IntVal 4294967296) (IntVal 4294967296)" $ operate Eq (IntVal 4294967296) (IntVal 4294967296) @?= Right TrueVal
  , testCase "operate Eq (IntVal 1) (IntVal 2)" $ operate Eq (IntVal 1) (IntVal 2) @?= Right FalseVal
  , testCase "operate Eq (IntVal 2) (IntVal 1)" $ operate Eq (IntVal 1) (IntVal 2) @?= Right FalseVal
  , testCase "operate Eq TrueVal (IntVal 1)" $ operate Eq TrueVal (IntVal 1) @?= Right FalseVal
  , testCase "operate Eq FalseVal NoneVal" $ operate Eq FalseVal NoneVal @?= Right FalseVal
  , testCase "operate Eq (ListVal []) (ListVal [NoneVal])" $ operate Eq (ListVal []) (ListVal [NoneVal]) @?= Right FalseVal
    -- Less
  , testCase "operate Less (IntVal 1) (IntVal 5)" $ operate Less (IntVal 1) (IntVal 5) @?= Right TrueVal
  , testCase "operate Less (IntVal (-2147483649)) (IntVal 2147483648)" $ operate Less (IntVal (-2147483649)) (IntVal 2147483648) @?= Right TrueVal
  , testCase "operate Less (IntVal (-1)) (IntVal 5)" $ operate Less (IntVal (-1)) (IntVal 5) @?= Right TrueVal
  , testCase "operate Less (IntVal 5) (IntVal (-1))" $ operate Less (IntVal 5) (IntVal (-1)) @?= Right FalseVal
  , testCase "*operate Less TrueVal (IntVal 2)" $ operate Less TrueVal (IntVal 2) @?= Left "Only integers allowed for Less Op"
  , testCase "*operate Less FalseVal (IntVal 2)" $ operate Less FalseVal (IntVal 2) @?= Left "Only integers allowed for Less Op"
  , testCase "*operate Less (StringVal \"Hello\") (IntVal 2)" $ operate Less (StringVal "Hello") (IntVal 2) @?= Left "Only integers allowed for Less Op"
  , testCase "*operate Less (ListVal []) (IntVal 2)" $ operate Less (ListVal []) (IntVal 2) @?= Left "Only integers allowed for Less Op"
    -- Greater
  , testCase "operate Greater (IntVal 1) (IntVal 5)" $ operate Greater (IntVal 1) (IntVal 5) @?= Right FalseVal
  , testCase "operate Greater (IntVal (-2147483649)) (IntVal 2147483648)" $ operate Greater (IntVal (-2147483649)) (IntVal 2147483648) @?= Right FalseVal
  , testCase "operate Greater (IntVal (-1)) (IntVal 5)" $ operate Greater (IntVal (-1)) (IntVal 5) @?= Right FalseVal
  , testCase "operate Greater (IntVal 5) (IntVal 1)" $ operate Greater (IntVal 5) (IntVal 1) @?= Right TrueVal
  , testCase "operate Greater (IntVal 2147483648) (IntVal (-2147483649))" $ operate Greater (IntVal 2147483648) (IntVal (-2147483649)) @?= Right TrueVal
  , testCase "operate Greater (IntVal 5) (IntVal (-1))" $ operate Greater (IntVal 5) (IntVal (-1)) @?= Right TrueVal
  , testCase "*operate Greater TrueVal (IntVal 2)" $ operate Greater TrueVal (IntVal 2) @?= Left "Only integers allowed for Greater Op"
  , testCase "*operate Greater FalseVal (IntVal 2)" $ operate Greater FalseVal (IntVal 2) @?= Left "Only integers allowed for Greater Op"
  , testCase "*operate Greater (StringVal \"Hello\") (IntVal 2)" $ operate Greater (StringVal "Hello") (IntVal 2) @?= Left "Only integers allowed for Greater Op"
  , testCase "*operate Greater (ListVal []) (IntVal 2)" $ operate Greater (ListVal []) (IntVal 2) @?= Left "Only integers allowed for Greater Op"
    -- In
  , testCase " operate In NoneVal (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal \"foo\", ListVal[IntVal 2]])" $ operate In NoneVal (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal "foo", ListVal[IntVal 2]]) @?= Right TrueVal
  , testCase " operate In TrueVal (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal \"foo\", ListVal[IntVal 2]])" $ operate In TrueVal (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal "foo", ListVal[IntVal 2]]) @?= Right TrueVal
  , testCase " operate In FalseVal (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal \"foo\", ListVal[IntVal 2]])" $ operate In FalseVal (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal "foo", ListVal[IntVal 2]]) @?= Right TrueVal
  , testCase " operate In (IntVal 1) (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal \"foo\", ListVal[IntVal 2]])" $ operate In (IntVal 1) (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal "foo", ListVal[IntVal 2]]) @?= Right TrueVal
  , testCase " operate In (StringVal \"foo\") (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal \"foo\", ListVal[IntVal 2]])" $ operate In (StringVal "foo") (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal "foo", ListVal[IntVal 2]]) @?= Right TrueVal
  , testCase " operate In (ListVal[IntVal 2]) (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal \"foo\", ListVal[IntVal 2]])" $ operate In (ListVal[IntVal 2]) (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal "foo", ListVal[IntVal 2]]) @?= Right TrueVal
  , testCase " operate In NoneVal (ListVal [])" $ operate In NoneVal (ListVal []) @?= Right FalseVal
  , testCase " operate In (StringVal \"bar\") (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal \"foo\", ListVal[IntVal 2]])" $ operate In (StringVal "bar") (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal "foo", ListVal[IntVal 2]]) @?= Right FalseVal
  , testCase " operate In (IntVal 3) (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal \"foo\", ListVal[IntVal 2]])" $ operate In (IntVal 3) (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal "foo", ListVal[IntVal 2]]) @?= Right FalseVal
  , testCase " operate In (IntVal 2) (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal \"foo\", ListVal[IntVal 2]])" $ operate In (IntVal 2) (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal "foo", ListVal[IntVal 2]]) @?= Right FalseVal
  , testCase " operate In (ListVal []) (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal \"foo\", ListVal[IntVal 2]])" $ operate In (ListVal []) (ListVal [NoneVal, TrueVal, FalseVal, IntVal 1, StringVal "foo", ListVal[IntVal 2]]) @?= Right FalseVal
  ]

applyTest = testGroup "apply tests" 
  [ testCase "apply \"print\" [NoneVal]" $ runComp (apply "print" [NoneVal]) [] @?= (Right NoneVal,["None"])
  , testCase "apply \"print\" [TrueVal]" $ runComp (apply "print" [TrueVal]) [] @?= (Right NoneVal,["True"])
  , testCase "apply \"print\" [FalseVal]" $ runComp (apply "print" [FalseVal]) [] @?= (Right NoneVal,["False"])
  , testCase "apply \"print\" [IntVal 3]" $ runComp (apply "print" [IntVal 3]) [] @?= (Right NoneVal,["3"])
  , testCase "apply \"print\" [StringVal \"foo\"]" $ runComp (apply "print" [StringVal "foo"]) [] @?= (Right NoneVal,["foo"])
  , testCase "apply \"print\" [ListVal []]" $ runComp (apply "print" [ListVal []]) [] @?= (Right NoneVal,["[]"])
  , testCase "apply \"print\" [ListVal [TrueVal, TrueVal, FalseVal]]" $ runComp (apply "print" [ListVal [TrueVal, TrueVal, FalseVal]]) [] @?= (Right NoneVal,["[True, True, False]"])
  , testCase "apply \"print\" [ListVal [TrueVal, TrueVal, ListVal [TrueVal, TrueVal, ListVal [TrueVal, TrueVal]], ListVal [TrueVal, TrueVal]]]" $ runComp (apply "print" [ListVal [TrueVal, TrueVal, ListVal [TrueVal, TrueVal, ListVal [TrueVal, TrueVal]], ListVal [TrueVal, TrueVal]]]) [] @?= (Right NoneVal,["[True, True, [True, True, [True, True]], [True, True]]"])
  , testCase "apply \"print\" [IntVal 42, StringVal \"foo\", ListVal [TrueVal, ListVal []], IntVal (-1)]" $ runComp (apply "print" [IntVal 42, StringVal "foo", ListVal [TrueVal, ListVal []], IntVal (-1)]) [] @?= (Right NoneVal,["42 foo [True, []] -1"])
    -- range
  , testCase "apply \"range\" [IntVal 3]" $ runComp (apply "range" [IntVal 3]) [] @?= (Right (ListVal [IntVal 0,IntVal 1,IntVal 2]),[])
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
  , testCase "apply \"range\" [IntVal 6, IntVal 0, IntVal (-2)]" $ runComp (apply "range" [IntVal 6, IntVal 0, IntVal (-2)]) [] @?= (Right (ListVal [IntVal 6,IntVal 4, IntVal 2]),[])
  , testCase "apply \"range\" [IntVal 10, IntVal 1, IntVal (-3)]" $ runComp (apply "range" [IntVal 10, IntVal 1, IntVal (-3)]) [] @?= (Right (ListVal [IntVal 10,IntVal 7, IntVal 4]),[])
  , testCase "apply \"range\" [IntVal 3, IntVal 10, IntVal 2]" $ runComp (apply "range" [IntVal 3, IntVal 10, IntVal 2]) [] @?= (Right (ListVal [IntVal 3,IntVal 5, IntVal 7, IntVal 9]),[])
  , testCase "apply \"range\" [IntVal 3, IntVal 11, IntVal 2]" $ runComp (apply "range" [IntVal 3, IntVal 11, IntVal 2]) [] @?= (Right (ListVal [IntVal 3,IntVal 5, IntVal 7, IntVal 9]),[])
  , testCase "apply \"range\" [IntVal 0, IntVal 6, IntVal (-2)]" $ runComp (apply "range" [IntVal 0, IntVal 6, IntVal (-2)]) [] @?= (Right (ListVal []),[])
  , testCase "apply \"range\" [IntVal 6, IntVal 0, IntVal (2)]" $ runComp (apply "range" [IntVal 6, IntVal 0, IntVal 2]) [] @?= (Right (ListVal []),[])
  , testCase "apply \"range\" [IntVal 6, IntVal 0, IntVal (2)]" $ runComp (apply "range" [IntVal 0, IntVal 0, IntVal 2]) [] @?= (Right (ListVal []),[])
  , testCase "apply \"range\" [IntVal 6, IntVal 0, IntVal (2)]" $ runComp (apply "range" [IntVal 0, IntVal 0, IntVal (-2)]) [] @?= (Right (ListVal []),[])
  , testCase "*apply \"range\" [IntVal 6, IntVal 0, IntVal (0)]" $ runComp (apply "range" [IntVal 6, IntVal 0, IntVal 0]) [] @?= (Left (EBadArg "Stepsize may not be 0"),[])
  -- not built in
  , testCase "*apply \"foo\" [NoneVal]" $ runComp (apply "foo" [NoneVal]) [] @?= (Left (EBadFun "foo"),[])
  ]

evalTest = testGroup "eval tests" 
  [ -- Const
    testCase "eval (Const (IntVal 1))" $ runComp (eval (Const (IntVal 1))) [] @?= (Right (IntVal 1),[])
  , testCase "eval (Const (IntVal 1))" $ runComp (eval (Const (IntVal 2147483648))) [] @?= (Right (IntVal 2147483648),[])
  , testCase "eval (Const (IntVal 1))" $ runComp (eval (Const (IntVal (-2147483649)))) [] @?= (Right (IntVal (-2147483649)),[])
    -- Var
  , testCase "eval (Var \"x\")" $ runComp (eval (Var "x")) [] @?= (Left (EBadVar "x"),[])
  , testCase "eval (Var \"x\")" $ runComp (eval (Var "x")) [("x", IntVal 4)] @?= (Right (IntVal 4),[])
  , testCase "eval (Var \"x\")" $ runComp (eval (Var "x")) [("x", IntVal 4), ("x", IntVal 5)] @?= (Right (IntVal 4),[])
  , testCase "eval (Var \"x\")" $ runComp (eval (Var "y")) [("x", IntVal 4), ("x", IntVal 5), ("y", IntVal 6)] @?= (Right (IntVal 6),[])
  , testCase "eval (Var \"x\")" $ runComp (eval (Var "y")) [("y", IntVal 6), ("x", IntVal 4), ("x", IntVal 5)] @?= (Right (IntVal 6),[])
    -- Oper - basic functionality and error handling as operate is tested thoroughly
    
    -- Compr
  , testCase "compr [x*x for x in range(4)]" $ runComp (eval (Compr (Oper Times (Var "x") (Var "x"))[CCFor "x" (Call "range" [Const (IntVal 4)])])) [] @?= (Right (ListVal [IntVal 0,IntVal 1,IntVal 4,IntVal 9]),[])
  , testCase "compr [1 | 1 == 1]" $ runComp (eval (Compr (Const (IntVal 1))[CCIf (Oper Eq (Const (IntVal 1)) (Const (IntVal 1)))])) [] @?= (Right (ListVal [IntVal 1]),[])
  , testCase "compr [x for x in range(4)]" $ runComp (eval (Compr (Var "x") [CCFor "x" (Call "range" [Const (IntVal 4)])])) [] @?= (Right (ListVal [IntVal 0,IntVal 1,IntVal 2,IntVal 3]),[])
  , testCase "compr [x*x for x in range(5)]" $ runComp (eval (Compr (Oper Times (Var "x") (Var "x")) [CCFor "x" (Call "range" [Const (IntVal 5)])])) [] @?= (Right (ListVal [IntVal 0,IntVal 1,IntVal 4,IntVal 9,IntVal 16]),[])
  , testCase "compr [j for i in range(2, n) for j in range(i*2, n*n, i)] n=4" $ runComp (eval (Compr (Var "j") [CCFor "i" (Call "range" [Const (IntVal 2),Var "n"]),CCFor "j" (Call "range" [Oper Times (Var "i")(Const (IntVal 2)),Oper Times (Var "n") (Var "n"),Var "i"])])) [("n", IntVal 4)] @?= (Right (ListVal [IntVal 4,IntVal 6,IntVal 8,IntVal 10,IntVal 12,IntVal 14, IntVal 6,IntVal 9,IntVal 12,IntVal 15]),[])
  , testCase "compr [2+x | for x in range(0,2) if x == 1]" $ runComp (eval (Compr (Oper Plus (Const (IntVal 2)) (Var "x")) [CCFor "x" (Call "range" [Const (IntVal 0), Const (IntVal 2)]), CCIf (Oper Eq (Var "x") (Const (IntVal 1)))])) [] @?= (Right (ListVal [IntVal 3]),[])
  , testCase "compr [[2+x] | for x in range(0,5) if x > 1]" $ runComp (eval (Compr (List [Var "x"]) [CCFor "x" (Call "range" [Const (IntVal 0), Const (IntVal 5)]), CCIf (Oper Greater (Var "x") (Const (IntVal 1)))])) [] @?= (Right (ListVal [ListVal [IntVal 4], ListVal [IntVal 5], ListVal [IntVal 6]]),[])
  ] 

stubbyTest = testGroup "Stubby tests" 
  [testCase "crash test" $ execute [SExp (Call "print" [Oper Plus (Const (IntVal 2)) (Const (IntVal 2))]), SExp (Var "hello")] @?= (["4"], Just (EBadVar "hello"))
  , testCase "execute misc.ast from handout" $ do pgm <- read <$> readFile "examples/misc.ast"; out <- readFile "examples/misc.out"; execute pgm @?= (lines out, Nothing)
  ]
