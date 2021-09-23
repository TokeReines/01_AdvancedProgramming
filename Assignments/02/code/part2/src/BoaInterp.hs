-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad
import Data.List

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String])}

instance Monad Comp where
  return a = Comp (\_e -> (Right a, []))
  m >>= f = Comp (\e -> case runComp m e of
                          (Left err, out) -> (Left err, out)
                          (Right a, out) -> case runComp (f a) e of
                                              (Left err, out2) -> (Left err, out <> out2)
                                              (Right b, out2) -> (Right b, out <> out2))

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort err = Comp (\_e -> (Left err, []))

look :: VName -> Comp Value
look v = Comp (\e -> case lookup v e of
                      Just x -> (Right x, [])
                      Nothing -> (Left $ EBadVar v, [])
                      )
-- Runs  the  computation m with x bound  to v,  
-- in  addition  to  any  othercurrent bindings
withBinding :: VName -> Value -> Comp a -> Comp a
withBinding x v m = Comp(\e -> runComp m ((x, v) : e)) -- Might give duplicate variables in env

output :: String -> Comp ()
output s = Comp (\_e -> (Right (), [s]))

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy NoneVal = False
truthy TrueVal = True
truthy FalseVal = False
truthy (IntVal x) = x /= 0
truthy (StringVal x) = (not . null) x
truthy (ListVal x) = (not . null) x

operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal x) (IntVal y) = (Right . IntVal) (x + y)
operate Plus _ _ = Left "Only integers allowed for Plus Op"
operate Minus (IntVal x) (IntVal y) = (Right . IntVal) (x - y)
operate Minus _ _ = Left "Only integers allowed for Minus Op"
operate Times (IntVal x) (IntVal y) = (Right . IntVal) (x * y)
operate Times _ _ = Left "Only integers allowed for Times Op"
operate Div (IntVal x) (IntVal y) 
  | y == 0 = Left "Division by 0 not allowed"
  | otherwise = (Right . IntVal) (x `div` y)
operate Div _ _ = Left "Only integers allowed for Div Op"
operate Mod (IntVal x) (IntVal y)
  | y == 0 = Left "Modulo by 0 not allowed"
  | otherwise = (Right . IntVal) (x `mod` y)
operate Mod _ _ = Left "Only integers allowed for Mod Op"
operate Eq x y = if x == y then Right TrueVal else Right FalseVal
operate Less (IntVal x) (IntVal y) = if x < y then Right TrueVal  else Right FalseVal
operate Less _ _ = Left "Only integers allowed for Less Op"
operate Greater (IntVal x) (IntVal y) = if x > y then Right TrueVal  else Right FalseVal
operate Greater _ _ = Left "Only integers allowed for Greater Op"
operate In x y
  | (ListVal ys) <- y = if x `elem` ys then Right TrueVal else Right FalseVal
  | otherwise = Left "Second argument to In has to be a list"


stringifyValue :: Value -> String
stringifyValue v
  | NoneVal <- v = "None"
  | TrueVal <- v = "True"
  | FalseVal <- v = "False"
  | (IntVal i) <- v = show i
  | (StringVal s) <- v = s
  | (ListVal []) <- v = "[]"
  | l@(ListVal vs) <- v = stringifyValues [l]

stringifyValues :: [Value] -> String
stringifyValues [] = ""
stringifyValues [v]
  | (ListVal x) <- v = "[" ++ intercalate ", " (map stringifyValue x) ++ "]"
  | otherwise = stringifyValue v
stringifyValues (v:vs) =  stringifyValues [v] ++ " " ++ stringifyValues vs

apply :: FName -> [Value] -> Comp Value
apply f v
  | f == "print" = do
      let s = stringifyValues v
      output s
      return NoneVal
  | f == "range" = case v of
    [IntVal x] ->
      -- Probably more effective that doing a recursive call
      do return (ListVal [IntVal x' | x' <- [0..x-1]])
    [IntVal x, IntVal y] ->
      -- Probably more effective that doing a recursive call
      do return (ListVal [IntVal x' | x' <- [x..y-1]])
    [IntVal x, IntVal y, IntVal z] ->
      if z == 0 then do abort (EBadArg "Stepsize may not be 0")
      -- Desc list with positive stepsize || Asc list with negative stepsize
      else if (x >= y && z > 0) || (x <= y && z < 0) then do return (ListVal [])
      else if z < 0 then do
        -- Range for desc list
        return (ListVal [IntVal x' | x' <- reverse [y+1..x], (x'-y) `mod` z == 0])
      else do
        -- Range for asc list. Here -1 in [x..y-1] makes sure to exclude the last element
        return (ListVal [IntVal x' | x' <- [x..y-1], (x'-x) `mod` z == 0])  --asc
    _ -> abort (EBadArg "Only integer values allowed as augments for function \"range\"")
  | otherwise = abort (EBadFun f)

-- Main functions of interpreter
-- eval e is the computation that evaluates the expression e in the current environment and returns its value
eval :: Exp -> Comp Value
eval (Const v) = return v
eval (Var v) = look v
eval (Oper o x y) = do
  x' <- eval x
  y' <- eval y
  case operate o x' y' of
    Left e -> abort (EBadArg e)
    Right v -> return v
eval (Not x) = do
  x' <- eval x
  if truthy x' then return FalseVal else return TrueVal
eval (List []) = return (ListVal [])
eval (List x) = do
  x' <- mapM eval x
  return (ListVal x')
eval (Call f xs) = do
  xs' <- mapM eval xs
  apply f xs'
eval (Compr e cc) = do
  a <- evalCompr cc e
  return (ListVal a)

evalCompr :: [CClause] -> Exp -> Comp [Value]
evalCompr [] e = do e' <- eval e; return [e']
evalCompr (cc:ccs) exp
  | CCFor v e <- cc = do
      e' <- eval e
      case e' of
        ListVal xs -> do
          a <- mapM (\x ->  withBinding v x (evalCompr ccs exp)) xs
          return (concat a)
          -- eval (Compr e ccs)
        _ -> do abort (EBadArg "CCFor clause needs to evaluate to a list")
  | CCIf e <- cc = do
    e' <- eval e
    if truthy e' then do evalCompr ccs exp
    else do return []

-- runComp (eval (Compr (Var "j") [CCFor "i" (Call "range" [Const (IntVal 2),Var "n"]),CCFor "j" (Call "range" [Oper Times (Var "i")(Const (IntVal 2)),Oper Times (Var "n") (Var "n"),Var "i"]), CCFor "l" (Call "range" [Oper Times (Var "i")(Const (IntVal 2)),Oper Times (Var "n") (Var "n"),Var "i"])])) [("n", IntVal 4)]
exec :: Program -> Comp ()
exec [] = return ()
exec (h:ss)
  | (SDef v e) <- h =
      do c <- eval e
         withBinding v c (exec ss)
  | (SExp e) <- h = do
      _v <- eval e
      exec ss

execute :: Program -> ([String], Maybe RunError)
execute p = case runComp (exec p) [] of
  (Left err, out) -> (out, Just err)
  (Right _, out) ->  (out, Nothing)