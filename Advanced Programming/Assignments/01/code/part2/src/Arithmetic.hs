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
-- Simple
showExp (Cst x)   = show x
showExp (Add x y) =  showExp x ++ "+" ++ showExp y
showExp (Sub x y) = "(" ++ showExp x ++ "-" ++ showExp y ++ ")"
showExp (Mul x@(Cst _) y@(Add _ _)) = showExp x ++ "*(" ++ showExp y ++ ")"
showExp (Mul x@(Add _ _) y@(Cst _)) = "(" ++ showExp x ++ ")*" ++ showExp y 
showExp (Mul x y) = "(" ++ showExp x ++ "*" ++ showExp y ++ ")"
showExp (Div x y) = "(" ++ showExp x ++ "`div`" ++ showExp y ++ ")"
showExp (Pow x y) = "(" ++ showExp x ++ "^" ++ showExp y ++ ")"
-- Full
showExp If{} = "Undefined"
showExp (Var _) = "Undefined"
showExp Let{} = "Undefined"
showExp Sum {} = "Undefined"

showExp' :: Exp -> String
showExp' e = case e of 
  (Cst x) -> show x
  (Add x y) ->  showExp' x ++ "+" ++ showExp' y

evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add x y) = evalSimple x + evalSimple y
evalSimple (Mul x y) = evalSimple x * evalSimple y

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
