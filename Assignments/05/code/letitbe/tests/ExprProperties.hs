module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E
-- import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as M
import Data.Map(Map)

-- From slide 22 - 30 in slides/09_property-based-testing-intro.pdf


-- instance Arbitrary Ident where
--    arbitrary = listOf(elements(elements(['a'..'z'] ++ ['A'..'Z'])))

instance Arbitrary Expr where
   arbitrary = expr

-- newtype UboundExpr = Expr deriving (Eq, Show, Read)

opGen :: Gen Op
opGen = elements [Plus, Minus, Times]

identGen :: Gen String
identGen = elements ["x", "y", "z", "bananaBread"]
 
prop_eval_simplify :: Expr -> Property
-- prop_eval_simplify x = E.evalTop(x) === E.evalTop(E.simplify(x))
prop_eval_simplify x = E.eval x (M.fromList [("x", 0), ("y", 0), ("z", 0), ("bananaBread", 0)]) === E.eval(E.simplify(x)) (M.fromList [("x", 0), ("y", 0), ("z", 0), ("bananaBread", 0)])

-- Testing let bindings with unbound variable still need to be "bound" else we get "Unknown identifier: "

-- ! Line 29 in ExprEval has + instead of -
expr = sized exprN
exprN 0 = fmap Const arbitrary
exprN n = oneof [ fmap Const arbitrary,
                  Var <$> identGen,
                  Oper <$> opGen <*> subexpr <*> subexpr,
                  Let <$> identGen <*> subexpr <*> subexpr]
              where subexpr = exprN (n `div` 2)

-- alrite -> i morgen/tirsdag:
-- Færdiggør haskell :P
-- Shrinking (også i erlang)
-- Test på version (erlang)
-- Evt prop measure  - er det stats? 👍🏻 h
