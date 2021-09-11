-- This is a skeleton file for you to edit

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

showExp :: Exp -> String
-- |Simple 2 + -1 -> Add (Cst 2) (Sub (Cst 0) (Cst 2)))
showExp (Cst x)   = show x
showExp (Add x y) = "(" ++ showExp x ++ "+" ++ showExp y ++ ")"
showExp (Sub (Cst 0) y) = "(-" ++ showExp y ++ ")"
showExp (Sub x y) = "(" ++ showExp x ++ "-" ++ showExp y ++ ")"
showExp (Mul x y) = "(" ++ showExp x ++ "*" ++ showExp y ++ ")"
showExp (Div x y) = "(" ++ showExp x ++ "`div`" ++ showExp y ++ ")"
showExp (Pow x y) = "(" ++ showExp x ++ "^" ++ showExp y ++ ")"
-- Full
showExp If{} = error "showExp is not compatible with type Exp If"
showExp (Var _) = error "showExp is not compatible with type Exp Var"
showExp Let{} = error "showExp is not compatible with type Exp Let"
showExp Sum {} = error "showExp is not compatible with type Exp Sum"

evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add x y) = evalSimple x + evalSimple y
evalSimple (Mul x y) = evalSimple x * evalSimple y
evalSimple (Div x y) 
  | evalSimple y == 0 = error "Division by zero not allowed"
  | otherwise = evalSimple x `div` evalSimple y
evalSimple (Pow x y) 
  | evalSimple y == 0 = 1
  | otherwise = evalSimple x ^ evalSimple y
evalSimple _ = error "Not defined evalSimple"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r = \v' -> if v' == v then Just n else r v

-- (Nothing)
-- a: if v == "a" then Just 2 else (Nothing)
-- b: if v == "b" then Just 3 else (if v == "a" then Just 2 else (Nothing))
-- c: if v == "c" then Just 4 else (if v == "b" then Just 3 else (if v == "a" then Just 2 else (Nothing)))

-- fromMaybe:: Maybe -> Exp
-- fromMaybe (Just a) = a
-- fromMaybe Nothing = 0 

evalFull :: Exp -> Env -> Integer
evalFull If {test=t, yes=y, no=n} e = if evalFull t e /= 0 then evalFull y e else evalFull n e
evalFull (Var v) e = case e v of 
    Just a -> a
    Nothing -> error "Variable not declared"
evalFull Let{var=v, def=d, body=b} e = evalFull b (extendEnv v 7 e)
-- evalFull (Sum{}) e = undefined
evalFull x _ = evalSimple x
evalFull (Mul x y) e = evalFull x e * evalFull y e
--
-- 0 = True
-- {...-1,1...} = True
e0 = initEnv
e1 = extendEnv "x" 1 e0
testEvalFullTrue = evalFull (If {test=Cst 1, yes=Cst 2, no=Cst 3}) e1
testEvalFullFalse = evalFull (If {test=Cst 0, yes=Cst 2, no=Cst 3}) e1

-- let x = 5 in (let x = 3 + 4 in x * x) + x
testEvalFullLet = evalFull (Let {var = "x", def = Add (Cst 3) (Cst 4),body = Mul (Var "x") (Var "x")}) initEnv

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr = undefined

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
