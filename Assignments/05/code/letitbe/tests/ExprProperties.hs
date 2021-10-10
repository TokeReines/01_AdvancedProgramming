module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E

-- From slide 22 - 30 in slides/09_property-based-testing-intro.pdf


-- instance Arbitrary Ident where
--    arbitrary = listOf(elements(elements(['a'..'z'] ++ ['A'..'Z'])))

instance Arbitrary Expr where
   arbitrary = expr

instance Arbitrary Op where
   arbitrary = oneof [Plus, Minus]

-- instance Arbitrary Ident where
--    arbitrary = oneof [Plus, Minus, Times]
 
prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = x === x

-- opOneof = oneof [Plus, Minus, Times]

expr = sized exprN
exprN 0 = fmap Const arbitrary
exprN n = oneof [ fmap Const arbitrary,
                  fmap Var arbitrary,
                  Oper Plus <$> subexpr <*> subexpr,
                  Oper Minus <$> subexpr <*> subexpr,
                  Oper Times <$> subexpr <*> subexpr,
                  Oper asd <$> subexpr <*> subexpr
                  -- Let Ident <$> subexpr <*> subexpr 
                     ]
              where subexpr = exprN (n `div` 2)
                    asd = oneof [Plus, Minus]