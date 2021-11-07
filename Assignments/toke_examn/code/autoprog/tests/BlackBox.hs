-- Sample black-box test suite. Feel free to adapt, or start from scratch.

-- Do NOT import from your ModImpl files here. These tests should work with
-- any implementation of the AutoProg APIs. Put any white-box tests in
-- suite1/WhiteBox.hs.
import Defs
import Parser
import Resolver
import Coder

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests -- testGroup "all" [tests]

tests = testGroup "Type tests" [
  testGroup "Parser" [
    -- Type constructor
    testCase "Empty type-constructor" $
      parseStringType "F" @?= Right (PTApp "F" []),
    testCase "Basic type-constructor" $
      parseStringType "F x" @?= Right (PTApp "F" [PTVar "x"]),
    testCase "Parenthesis type-constructor" $
      parseStringType "F (x)" @?= Right (PTApp "F" [PTVar "x"]),   
    testCase "Nested type-constructor" $
      parseStringType "F G x" @?= Right (PTApp "F" [PTApp "G" [], PTVar "x"]),   
    testCase "Tuple type-constructor" $
      parseStringType "F (x, y)" @?= Right (PTApp "F" [PTApp "(,)" [PTVar "x", PTVar "y"]]),

    -- Infix arrow
    testCase "Basic infix-arrow" $
      parseStringType "x -> y" @?= Right (PTApp "(->)" [PTVar "x", PTVar "y"]),
    testCase "Missing right side infix-arrow" $
      parseStringType "x ->" @?= Left "no parse",    
    testCase "Missing left side infix-arrow" $
      parseStringType "-> y" @?= Left "no parse", 
    testCase "Parenthesis infix-arrow" $
      parseStringType "x -> (y -> z) -> z" @?= Right (PTApp "(->)" [PTVar "x",PTApp "(->)" [PTApp "(->)" [PTVar "y",PTVar "z"],PTVar "z"]]),
    testCase "Tuple infix-arrow" $
      parseStringType "(x, x) -> (x, x)" @?= Right (PTApp "(->)" [PTApp "(,)" [PTVar "x",PTVar "x"],PTApp "(,)" [PTVar "x",PTVar "x"]]),

    -- Binding tightness
    testCase "Type-constructor binding vs infix arrow" $
      parseStringType "F x y -> z" @?= Right (PTApp "(->)" [PTApp "F" [PTVar "x",PTVar "y"],PTVar "z"]),

    -- comments
    testCase "Unclosed comments" $
      parseStringType "F x{-{- -} y -> z" @?= Left "no parse",
    testCase "Comment as whitespace" $
      parseStringType "F x{--}y -> z" @?= Right (PTApp "(->)" [PTApp "F" [PTVar "x",PTVar "y"],PTVar "z"]),
    testCase "Double comment" $
      parseStringType "F x{-{--}-}y -> z" @?= Left "no parse",

    -- Declarations
    testCase "Type declaration" $
      parseStringTDeclz "type T a = a -> a" @?= Right [TDSyn ("T",["a"]) (PTApp "(->)" [PTVar "a",PTVar "a"])],
    testCase "Type declaration mix newtype" $
      parseStringTDeclz "newtype T a = a -> a" @?= Left "no parse",
    testCase "Type declaration mix data" $
      parseStringTDeclz "data T a = a -> a" @?= Left "no parse",

    testCase "Newtype declaration" $
      parseStringTDeclz "newtype T a = F { x :: a }" @?= Right [TDRcd ("T",["a"]) "F" [("x",PTVar "a")]],
    testCase "Newtype declaration arrow" $
      parseStringTDeclz "newtype T a = F { x :: a -> b}" @?= Right [TDRcd ("T",["a"]) "F" [("x", PTApp "(->)" [PTVar "a", PTVar "b"])]],
    testCase "Newtype declaration mix type" $
      parseStringTDeclz "type T a = F { x :: a }" @?= Left "no parse",

    testCase "Data declaration" $
      parseStringTDeclz "data T a = F { x :: a }" @?= Right [TDRcd ("T",["a"]) "F" [("x",PTVar "a")]],
    testCase "Data declaration, multiple" $
      parseStringTDeclz "data T a = F { x, y :: a }" @?= Right [TDRcd ("T",["a"]) "F" [("x",PTVar "a"),("y",PTVar "a")]],
    testCase "Data declaration, arrow" $
      parseStringTDeclz "data T a = F { x :: a -> a}" @?= Right [TDRcd ("T",["a"]) "F" [("x",PTApp "(->)" [PTVar "a",PTVar "a"])]],
    testCase "Data declaration" $
      parseStringTDeclz "data T a = F { x :: a, y :: a }" @?= Right [TDRcd ("T",["a"]) "F" [("x",PTVar "a"),("y",PTVar "a")]]
  ]]

-- tests2 = testGroup "Minimal tests" [
--   testGroup "Parser" [
--     testCase "...Type" $
--       parseStringType "a -> a" @?= Right pt0,
--     testCase "...TDeclz" $
--       parseStringTDeclz "type T a = a -> a" @?= Right [td0]
--   ]
--   -- testGroup "Resolver" [
--   --   testCase "resolve" $
--   --     resolve tce0 (\x -> return $ STVar (x++"'")) pt0 @?= Right st0,
--   --   testCase "declare" $
--   --     do tce <- declare [td0]
--   --        tf <- case lookup "T" tce of Just tf -> return tf; _ -> Left "no T"
--   --        tf [STVar "a'"]
--   --     @?= Right st0
--   -- ],
--   -- testGroup "Coder" [
--   --   testCase "pick" $
--   --     do n <- pick [0,3]
--   --        if n > 0 then return n
--   --        else do m <- pick [4,0]
--   --                if m > 0 then return m else pick []
--   --     @?= tr0,
--   --   testCase "solutions" $
--   --     solutions tr0 10 Nothing @?= [3,4],
--   --   testCase "produce" $
--   --     do e <- dfs (produce [] st0)
--   --        return $ case e of
--   --                   Lam x (Var x') | x' == x -> e0
--   --                   _ -> e 
--   --     @?= [e0]
--     ]
--  where pt0 = PTApp "(->)" [PTVar "a", PTVar "a"]
--        td0 = TDSyn ("T", ["a"]) pt0
--        st0 = STArrow (STVar "a'") (STVar "a'")
--        tr0 = Choice [Choice [Found 4, Choice []], Found 3]
--        dfs (Found a) = [a]
--        dfs (Choice ts) = concatMap dfs ts
--        e0 = Lam "X" (Var "X")
