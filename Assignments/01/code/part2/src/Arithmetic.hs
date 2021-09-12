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
import Debug.Trace
import Definitions
import Data.Either (fromRight)

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
evalSimple (Sub x y) = evalSimple x - evalSimple y
evalSimple (Mul x y) = evalSimple x * evalSimple y
evalSimple (Div x y) 
  | evalSimple y == 0 = error "Division by zero not allowed"
  | otherwise = evalSimple x `div` evalSimple y
evalSimple (Pow x y) 
  | evalSimple y == 0 = 1
  | otherwise = evalSimple x ^ evalSimple y
evalSimple _ = error "Unmatched exp in evalSimple"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r = \v' -> if v' == v then Just n else r v

-- For debugging: evalFull x _  | trace ("evalFull: " ++ show x) False = undefined
evalFull :: Exp -> Env -> Integer
evalFull If {test=t, yes=y, no=n} e = if evalFull t e /= 0 then evalFull y e else evalFull n e
evalFull (Var v) e = case e v of 
    Just a -> a
    Nothing -> error "Variable not declared"
evalFull Let{var=v, def=d, body=b} e = evalFull b (extendEnv v (evalFull d e) e)
evalFull (Sum{var=v, from=f, to=t, body=b}) e
    | from < to = (evalFull b extE) + (evalFull Sum{var=v, from=incF, to=t, body=b} extE)
    | from == to = evalFull b extE
    | otherwise = evalFull (Cst 0) extE
    where from = evalFull f e
          to = evalFull t e
          extE = extendEnv v from e
          incF = Add f (Cst 1)
evalFull (Add x y) e = evalFull x e + evalFull y e
evalFull (Sub x y) e = evalFull x e - evalFull y e
evalFull (Mul x y) e = evalFull x e * evalFull y e
evalFull (Pow x y) e = evalFull x e ^ evalFull y e
evalFull (Div x y) e 
    | evalFull y e == 0 = error "Division by zero not allowed"
    | otherwise = evalFull x e `div` evalFull y e
evalFull x _ = evalSimple x

-- fromEither :: Either ArithError Integer ->
-- https://stackoverflow.com/questions/156013/haskell-syntax-for-a-case-expression-in-a-do-block
evalErr :: Exp -> Env -> Either ArithError Integer
-- evalErr If {test=t, yes=y, no=n} e = if evalErr t e /= 0 then evalErr y e else evalErr n e
-- evalErr (Var v) e = case e v of 
--     Just a -> Righta
--     Nothing -> Left (EBadVar v)
evalErr (Cst x) e | trace ("Cst: " ++ show x) False = undefined
evalErr (Cst x) _ = Right x
evalErr (Add x y) e | trace ("Add: " ++ show x ++ " " ++ show y) False = undefined
evalErr (Add x y) e
  | (Left _) <- first = first
  | (Left _) <- second = second
  | (Right _) <- first
  , (Right _) <- second 
  = Right ((fromRight 1 first) + (fromRight 1 second))
  where first  = evalErr x e
        second = evalErr y e
evalErr (Sub x y) e | trace ("Sub: " ++ show x ++ " " ++ show y) False = undefined
evalErr (Sub x y) e
  | (Left _) <- first = first
  | (Left _) <- second = second
  | (Right _) <- first
  , (Right _) <- second 
  = Right ((fromRight 1 first) - (fromRight 1 second))
  where first  = evalErr x e
        second = evalErr y e
evalErr (Pow x y) e | trace ("Pow: " ++ show x ++ " " ++ show y) False = undefined
evalErr (Pow x y) e 
  | (Left _) <- first = first
  | (Left _) <- second = second
  | (fromRight 1 second) <= 0 = Left ENegPower
  | (Right _) <- first
  , (Right _) <- second 
  = Right ((fromRight 1 first) ^ (fromRight 1 second))
  where first  = evalErr x e
        second = evalErr y e
evalErr (Div x y) e | trace ("Div: " ++ show x ++ " " ++ show y) False = undefined
evalErr (Div x y) e 
  | (Left _) <- first = first
  | (Left _) <- second = second
  | (Right 0) <- second = Left EDivZero
  | (Right _) <- first
  , (Right _) <- second 
  = Right ((fromRight 1 first) `div` (fromRight 1 second))
  where first  = evalErr x e
        second = evalErr y e

-- https://stackoverflow.com/a/46944385

-- evalErr (Div x y) e = do
--     first <- evalErr x e
--     second <- evalErr y e
--     return (case first of
--         (Right x) -> Right x
--         otherwise -> Left EDivZero)

-- evalErr (Add x y) e = evalErr x e
-- evalErr (Add x y) e 
--     | (Right _) <- x = Right first
--     | otherwise = Left ENegPower
--     where 
--         first = evalErr x e
--         second = evalErr y e
-- evalErr (Add x y) e 
--     | (Add Right x _ y) = evalErr  first
--     | otherwise = Left ENegPower
--     where 
--         first = evalErr x e
--         second = evalErr y e
-- evalErr x _ = Right (evalSimple x)

-- optional parts (if not attempted, leave them unmodified)
-- func = λx ->
      -- let { k = bat x } in
      -- let { j = baz k } in
      -- +
      --   (+ (bar j) j)
      --   k
-- (Add (Div (Cst 2) (Cst 0)) (Pow (Cst 4) (Sub (Cst 0) (Cst 1)))) should show ((2*3)+4)
-- testX = evalErr(Add (Cst 2) (Cst 2)) initEnv -- should show ((2*3)+4) 
showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined

-- tests
--

