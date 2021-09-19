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

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }
instance Monad Comp where
  return a = Comp (\_e -> (Right a, []))
  Comp m >>= f = Comp (\e -> 
    case m e of
      (Right a, out) -> runComp (f a) e
      (Left re, out) -> (Left re, out))

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
withBinding = undefined

output :: String -> Comp ()
output = undefined

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy = undefined

operate :: Op -> Value -> Value -> Either String Value
operate = undefined

apply :: FName -> [Value] -> Comp Value
apply = undefined

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval = undefined

exec :: Program -> Comp ()
exec = undefined

execute :: Program -> ([String], Maybe RunError)
execute = undefined
