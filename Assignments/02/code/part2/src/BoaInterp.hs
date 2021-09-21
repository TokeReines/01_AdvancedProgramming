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
  -- m >>= f = Comp (\e -> case runComp m e of
  --                       (Left err, out) -> (Left err, out)
  --                       (Right a, _) -> runComp (f a) e)
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
operate Minus (IntVal x) (IntVal y) = (Right . IntVal) (x - y)
operate Minus _ _ = Left "Only integers allowed for Minus Op"
operate Times (IntVal x) (IntVal y) = (Right . IntVal) (x * y)
operate Times _ _ = Left "Only integers allowed for Times Op"
-- Right(IntVal(FuncA(FuncB(x + y))))
-- operate Plus (StringVal x StringVal y) = Left x ++ y
operate _ _ _ = undefined
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
        return (ListVal [IntVal x' | x' <- reverse [y..x-1], (x'-y) `mod` z == 0])
      else do
        -- Range for asc list
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
-- (Compr (Var "x")[CCFor "x" (Call "range" [Const (IntVal 10)])])
-- [e(x*x) cc([for x in range(10)]) if x < 5]
eval (Compr e [cc])
  -- | cc -> [eval ccs -> withBinding var value -> eval ccss -> withBinding var' value']
  | CCFor v e' <- cc = do
    val <- eval e'
    case val of 
      ListVal xs -> do 
        a <- mapM (\x -> withBinding v x (eval e)) xs
        return (ListVal a)
      _ -> do abort (EBadArg "CCFor clause needs to evaluate to a list")
  -- | CCIf e <- cc = do
  --   e' <- eval e
  --   if truthy e' then do 
  --     return TrueVal
  --   else do 
  --     return FalseVal  
          
-- eval (Compr e ccs)
--   | CCFor v e' <- cc = do
--     val <- eval e'
--     case val of 
--       ListVal xs -> do 
--         a <- mapM (\x -> withBinding v x (eval e)) xs
--         return (ListVal a)
--       _ -> do abort (EBadArg "CCFor clause needs to evaluate to a list")
--   | CCIf e <- cc = do
--     e' <- eval e
--     if truthy e' then do 
--       return TrueVal
--     else do 
--       return FalseVal
eval _ = undefined 

-- (Compr (Var "j") [CCFor "i" (Call "range" [Const (IntVal 2),Var "n"]), Var "i"])
--       (Compr (Oper Times (Var "x") (Var "x"))[CCFor "x" (Call "range" [Const (IntVal 10)])])
-- eval (CCFor v e) = do 
--   vs <- eval e 
--   return (mapM (\i -> withBinding v i (eval ss) vs))
  -- case vs of
  --   List [x:xs] -> do return undefined
  --   _ -> do return undefined
-- eval (CCIf e) = undefined



-- execute (Call "range" [Const (IntVal 2),Var "n"])
-- Likewise, exec p is the computation arising from executing the program (or program fragment) p, with 
-- no nominal return value, but with any side effects in p still taking place in the computation.
exec :: Program -> Comp () -- Program = [Stmt]
exec [] = return ()
-- exec [st]
--   | (SDef v e) <- st = 
--     do eval e
--        exec []      
--   | (SExp e) <- st = 
--     do eval e
--        exec []
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

-- >>> list(range(10,-10,1))
-- []
-- >>> list(range(-10, 10, 1))
-- [-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- >>> list(range(-10, 10, -1))
-- []
-- if z < 0 
--   then let a = 
--   else let a = [min..max]
--   in [IntVal x | x <- a, x `mod` z == 0]
--foldl (flip (:)) [] [min..max] 

-- (x >= y && z > 0) || (x <= y && z < 0) = ListVal []
-- z == 0 = EBadArg "Stepsize may not be 0"[min x y..max x y - 1]

-- runComp (Call "range" [Const (IntVal 10)]) [] 