module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E
-- import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as M
import Data.Map(Map)

instance Arbitrary Expr where
   arbitrary = expr

opGen :: Gen Op
opGen = elements [Plus, Minus, Times]

identGen :: Gen String
identGen = elements ["x", "y", "z", "q"]
-- Simplify doesnt use the values....
defaultEnv = M.fromList [("x", 0), ("y", 0), ("z", 0), ("q", 0)]

prop_eval_simplify :: Expr -> Property
-- prop_eval_simplify x = E.evalTop x === E.evalTop(E.simplify x)
-- ! Testing let bindings with unbound variable still needs the varibles to be 
-- ! bound when we evaluate else we get "Unknown identifier: "
-- ! Thus we make sure to bind the variables in the environment as a precondition 
prop_eval_simplify x = E.eval x defaultEnv === E.eval (E.simplify x) defaultEnv


expr = sized exprN
exprN 0 = fmap Const arbitrary
exprN n = oneof [ fmap Const arbitrary,
                  Var <$> identGen,
                  Oper <$> opGen <*> subexpr <*> subexpr,
                  Let <$> identGen <*> subexpr <*> subexpr]
              where subexpr = exprN (n `div` 2)

