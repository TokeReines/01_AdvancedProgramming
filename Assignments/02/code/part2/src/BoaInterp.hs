-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
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
  Comp m >>= f = Comp (\e -> 
    case m e of
      (Right a, out) -> runComp (f a) e
      (Left re, out) -> (Left re, out))
      
  -- Comp m >>= f = Comp (\e -> let (a, o) = runComp m e
  --                                (a', o') = runComp (f a) e
  --                             in case (a' o') of
  --     (Right a, out) -> (Right a, )
  --     (Left re, out) -> (Left re, out))

--  Comp >>= f = Comp (\e -> do (a, out) <- runComp m e
--                           Right ())

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort err = Comp (\e -> (Left err, []))

look :: VName -> Comp Value
look v = Comp (\e -> case lookup v e of 
                      Just x -> (Right x, [])
                      Nothing -> (Left $ EBadVar v, [])
                      )

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding x v m = undefined -- let e' = v in m e

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
-- Right(IntVal(FuncA(FuncB(x + y))))
-- operate Plus (StringVal x StringVal y) = Left x ++ y
operate Plus _ _ = Left "Only integers allowed for addition"
operate Minus x y= undefined
operate Times x y =undefined
operate Div x y = undefined
operate Mod x y = undefined
operate Eq x y = undefined
operate Less x y = undefined
operate Greater x y = undefined
operate In x y = undefined

apply :: FName -> [Value] -> Comp Value
apply = undefined

-- Main functions of interpreter
-- eval e is the computation that evaluates the expression e in the current environment and returns its value
eval :: Exp -> Comp Value
eval (Const v) = Comp (\e -> (Right v, [])) -- Const List [Const Intval 1] or Const (IntVal 2)
-- eval Var v = undefined
-- eval Oper Op Exp Exp = undefined
-- eval Not Exp = undefined
-- eval Call f [Exp] 
-- eval Call f [] = undefined
-- eval Call f [x]
--   | f == "print" = 
-- eval Call f [x:xs] 
--   | f == "print" = 
-- eval List [Exp] = undefined
-- eval Compr Exp [CClause] = undefined

-- Likewise, exec p is the computation arising from executing the program (or program fragment) p, with no nominal return value, but with any side effects in p still taking
-- place in the computation.
exec :: Program -> Comp () -- Program = [Stmt]
exec p = undefined

-- Finally, execute p explicitly returns the list of output lines, and the error message
-- (if relevant) resulting from executing p in the initial environment, which contains
-- no variable bindings. (For implementing execute (only), you are allowed to use the 
-- runComp projection of the monad type.)
execute :: Program -> ([String], Maybe RunError)
execute p = undefined
--  (x, y) <- runComp exec []

          