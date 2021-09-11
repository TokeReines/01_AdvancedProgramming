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

-- showExp' :: Exp -> String
-- showExp' e = case e of 
--   (Cst x) -> show x
--   (Add x y) ->  showExp' x ++ "+" ++ showExp' y

-- showExp'' :: Exp -> String
-- showExp'' e 
--   | Cst x <- e =  show x
--   | Add x y <- e =  showExp' x ++ "+" ++ showExp' y

evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add x y) = evalSimple x + evalSimple y
evalSimple (Mul x y) = evalSimple x * evalSimple y
evalSimple (Div x y) 
  | evalSimple y == 0 = error "Divison by zero not allowed"
  | otherwise = evalSimple x `div` evalSimple y
evalSimple (Pow x y) 
  | evalSimple y == 0 = 1
  | otherwise = evalSimple x ^ evalSimple y

extendEnv :: VName -> Integer -> Env -> Env
extendEnv = undefined

evalFull :: Exp -> Env -> Integer
evalFull = undefined

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr = undefined

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
