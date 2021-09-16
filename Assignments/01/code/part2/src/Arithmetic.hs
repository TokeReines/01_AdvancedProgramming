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
showExp (Cst x)
  | x < 0 = "(" ++ show x ++ ")" -- Add parenthesis to negative integers
  | otherwise = show x
showExp (Add x y) = "(" ++ showExp x ++ "+" ++ showExp y ++ ")"
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
evalSimple (Sub x y) = evalSimple x - evalSimple y
evalSimple (Mul x y) = evalSimple x * evalSimple y
evalSimple (Div x y) 
  | evalSimple y == 0 = error "Division by zero not allowed"
  | otherwise = evalSimple x `div` evalSimple y
evalSimple (Pow x y)
  | evalSimple y == 0 = evalSimple x * 0 + 1
  | otherwise = evalSimple x ^ evalSimple y
evalSimple _ = error "Unmatched exp in evalSimple"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r = \v' -> if v' == v then Just n else r v'


evalFull :: Exp -> Env -> Integer
evalFull (Cst x) _ = x
evalFull If {test=t, yes=y, no=n} e = if evalFull t e /= 0 then evalFull y e else evalFull n e
evalFull (Var v) e = case e v of 
    Just a -> a
    Nothing -> error "Variable not declared"
evalFull Let{var=v, def=d, body=b} e = evalFull b (extendEnv v (evalFull d e) e)
evalFull Sum{var=v, from=f, to=t, body=b} e
    | from < to = evalFull b extE + evalFull Sum{var=v, from=incF, to=t, body=b} e
    | from == to = evalFull b extE
    | otherwise = evalFull (Cst 0) extE
    where from = evalFull f e
          to = evalFull t e
          extE = extendEnv v from e
          incF = Add f (Cst 1) -- Increments f for the recursive call of sum
evalFull (Add x y) e = evalFull x e + evalFull y e
evalFull (Sub x y) e = evalFull x e - evalFull y e
evalFull (Mul x y) e = evalFull x e * evalFull y e
evalFull (Div x y) e 
    | evalFull y e == 0 = error "Division by zero not allowed"
    | otherwise = evalFull x e `div` evalFull y e
evalFull (Pow x y) e
  | evalFull y e == 0 = evalFull x e * 0 + 1 
  | otherwise = evalFull x e ^ evalFull y e

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst x) _ = Right x
evalErr (Add x y) e = do 
  x' <- evalErr x e
  y' <- evalErr y e
  return (x'+y')
evalErr (Sub x y) e = do 
  x' <- evalErr x e
  y' <- evalErr y e
  return (x'-y')
evalErr (Mul x y) e = do
  x' <- evalErr x e
  y' <- evalErr y e
  return (x'*y')
evalErr (Div x y) e = do
  x' <- evalErr x e
  y' <- evalErr y e
  if y' /= 0 then return (x' `div` y') else Left EDivZero
evalErr (Pow x y) e = do 
  x' <- evalErr x e
  y' <- evalErr y e
  if y' >= 0 then return (x' ^ y') else Left ENegPower
evalErr If {test=t, yes=y, no=n} e = do 
  t' <- evalErr t e
  if t' /= 0 then do
    evalErr y e
  else do
    evalErr n e
evalErr (Var x) e = 
    case e x of 
      Just a -> Right a
      Nothing -> Left (EBadVar x)
evalErr Let{var=v, def=d, body=b} e = do 
  d' <- evalErr d e
  evalErr b (extendEnv v d' e)
evalErr Sum {var=v, from=f, to=t, body=b} e = do 
  f' <- evalErr f e
  t' <- evalErr t e
  if f' < t' then do
    b' <- evalErr b (extendEnv v f' e)
    s' <- evalErr Sum{var=v, from=Add f (Cst 1), to=t, body=b} e
    return (b' + s')
  else if f' == t' then do
    evalErr b (extendEnv v f' e)
  else do
    evalErr (Cst 0) (extendEnv v f' e)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined