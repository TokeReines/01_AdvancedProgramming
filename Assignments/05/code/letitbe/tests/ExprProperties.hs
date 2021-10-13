module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E
import qualified Data.Map.Strict as M
import Data.Map(Map)

instance Arbitrary Expr where
   arbitrary = expr

opGen :: Gen Op
opGen = elements [Plus, Minus, Times]

identGen :: Gen String
identGen = elements ["x", "y", "z", "q"]

identGenFreq :: Gen String
identGenFreq = frequency [
  (8, elements ["x"]),
  (1, elements ["z", "q"])]


identGenNotOf :: Ident -> Gen String
identGenNotOf i = identGen `suchThat`  (/= i)

prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = E.eval x (mapDefaultEnv x) === E.eval (E.simplify x) (mapDefaultEnv x)

prop_measure_eval_fail :: Expr -> Property
prop_measure_eval_fail x = collect (case E.evalTop x of
                                      Right _ -> "Eval success"
                                      Left _ -> "Eval Failed") $ 
                            E.eval x (mapDefaultEnv x) === E.eval (E.simplify x) (mapDefaultEnv x)

expr = sized exprN
exprN 0 = fmap Const arbitrary
exprN n = frequency [ (2, fmap Const arbitrary),
                      (1, Var <$> identGenFreq),
                      (2, Oper <$> opGen <*> subexpr <*> subexpr),
                      (2, Let <$> identGenFreq <*> subexpr <*> subexpr)]
              where subexpr = exprN (n `div` 2)


usedInLetBinding :: Expr -> [Ident]
usedInLetBinding (Var _) = []
usedInLetBinding (Const _) = []
usedInLetBinding (Oper _ e1 e2) = usedInLetBinding e1 ++ usedInLetBinding e2
usedInLetBinding (Let _ e1 e2) = usedVariables e1 ++ usedInLetBinding e2

usedVariables :: Expr -> [Ident]
usedVariables (Var v) = [v]
usedVariables (Const _) = []
usedVariables (Oper _ e1 e2) = usedVariables e1 ++ usedVariables e2
usedVariables (Let _ e1 body) = usedVariables e1 ++ usedVariables body

mapDefaultEnv :: Expr  -> Map Ident Int
mapDefaultEnv e = M.fromList [(i, 0) | i <- usedInLetBinding e]