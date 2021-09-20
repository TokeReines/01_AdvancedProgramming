-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output, stringifyValues,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String])}

instance Monad Comp where
  return a = Comp (\_e -> (Right a, []))
  m >>= f = Comp (\e -> do (_, out) <- runComp m e
                           (_, out') <- runComp (f a) e
                           Right (_, out <> out'))
  -- m >>= f = RWSE (\r s0 ->  do (a, w1, s1) <- runRWSE m r s0
  --                              (b, w2, s2) <- runRWSE (f a) r s1
  --                              Right (b, w1 <> w2, s2))

  -- m >>= f = Comp (\e -> case runComp m e of
  --                         (Left err, out) -> (Left err, out)
  --                         (Right a, _) -> runComp (f a) e)

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
withBinding x v m = Comp(\e -> runComp m ([(x, v)] ++ e)) -- Might give duplicate variables in env

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
-- Right(IntVal(FuncA(FuncB(x + y))))
-- operate Plus (StringVal x StringVal y) = Left x ++ y
operate _ _ _ = undefined
-- operate Minus (IntVal x) (IntVal y) = (Right . IntVal) (x - y)
-- operate Times (IntVal x) (IntVal y) = (Right . IntVal) (x * y)
-- operate Div (IntVal x) (IntVal y) = (Right . IntVal) (x `div` y)
-- operate Mod (IntVal x) (IntVal y) = (Right . IntVal) (x `mod` y)
-- operate Eq (IntVal x) (IntVal y)
--   | x == y = Right TrueVal
--   | otherwise = Right FalseVal
-- operate Less (IntVal x) (IntVal y)   
--   | x < y = Right TrueVal
--   | otherwise = Right FalseVal
-- operate Greater (IntVal x) (IntVal y)    
--   | x > y = Right TrueVal
--   | otherwise = Right FalseVal
-- -- Many more cases here. Haskell doesn't allow elem 1 ["hej"], but python does
-- operate In (TrueVal) (ListVal l) 
--   | elem TrueVal l = Right TrueVal
--   | otherwise = Right FalseVal


stringifyValues :: [Value] -> String
stringifyValues [] = ""
stringifyValues [v]
  | NoneVal <- v = "None"
  | TrueVal <- v = "True"
  | FalseVal <- v = "False"
  | (IntVal i) <- v = show i
  | (StringVal s) <- v = s
  | (ListVal []) <- v = "[]"
  | (ListVal [x]) <- v = "[" ++ stringifyValues [x]  ++ "]"
  | (ListVal (x:xs)) <- v = "[" ++ stringifyValues [x]  ++ ", " ++ stringifyValues xs ++ "]"
stringifyValues (v:vs) =  stringifyValues [v] ++ " " ++ stringifyValues vs


-- [IntVal 42, StringVal "foo", ListVal [TrueVal, ListVal []], IntVal (-1)]

apply :: FName -> [Value] -> Comp Value
apply f v
  | f == "print" = do
      let s = stringifyValues v
      output s
      return NoneVal
  | f == "range" = undefined
  | otherwise = abort (EBadFun f)
-- | f == "range" = case v of 
--   [IntVal x] -> let v = map (\x -> IntVal x) [0..1] 
--                 in Comp (\e -> (Right (ListVal v), []))
--   [IntVal x, IntVal y] -> let v = map (\x -> IntVal x) [x..y] 
--                           in Comp (\e -> (Right (ListVal v), []))
--   [IntVal x, IntVal y, IntVal z] -> let v = map (\x -> IntVal x) [z - 1, x..y]
--                                     in Comp (\e -> (Right (ListVal v), [])) -- [0, 1..3] -> [1,2,3]. z is 0-indexed]
-- | otherwise = undefined
  
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
-- eval Call f [x] = do
--   x' <- eval x

-- eval Call f [x:xs] 
--   | f == "print" = 
-- eval List [Exp] = undefined
-- eval Compr Exp [CClause] = undefined
eval _ = undefined 

-- Likewise, exec p is the computation arising from executing the program (or program fragment) p, with 
-- no nominal return value, but with any side effects in p still taking place in the computation.
exec :: Program -> Comp () -- Program = [Stmt]
exec [] = return ()
exec [st]
  | (SDef v e) <- st = 
    do eval e
       exec []      
  | (SExp e) <- st = 
    do eval e
       exec []
exec (h:ss)
  | (SDef v e) <- h = 
      do c <- eval e
         withBinding v c (exec ss)           
  | (SExp e) <- h = do 
      v <- eval e
      exec ss
         

-- [Stmt: SDef "x" = 2+2, Stmt: SExp Add "x" 2]

-- withBinding :: VName -> Value -> Comp a -> Comp a
-- Finally, execute p explicitly returns the list of output lines, and the error message
-- (if relevant) resulting from executing p in the initial environment, which contains
-- no variable bindings. (For implementing execute (only), you are allowed to use the 
-- runComp projection of the monad type.)
execute :: Program -> ([String], Maybe RunError)
execute p = case runComp (exec p) [] of
  (Left err, out) -> (out, Just err)
  (Right a, out) ->  (out, Nothing)