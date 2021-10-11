module ExprEval where

import ExprAst
import qualified Data.Map.Strict as M
import Data.Map(Map)

type Env = Map String Int

oper :: Op -> (Int -> Int -> Int)
oper Plus = (+)
oper Minus = (-)
oper Times = (*)

eval :: Expr -> Env -> Either String Int
eval (Const n) env = return n
eval (Oper op x y) env = (oper op) <$> eval x env <*> eval y env
eval (Var v) env = case M.lookup v env of
                     Nothing -> Left ("Unknown identifier: "++v)
                     Just val -> return val
eval (Let v e body) env = do
  val <- eval e env
  eval body $ M.insert v val env

evalTop e = eval e M.empty

simplify e =
  case e of
    Oper Plus (Const 0) c@(Const c1) -> c
    Oper Plus c@(Const c1) (Const 0) -> c
    Oper Plus (Const c1) (Const c2) -> Const(c1+c2)
    Oper Minus (Const 0) c@(Const c1) -> c
    Oper Minus c@(Const c1) (Const 0) -> c
    Oper Minus (Const c1) (Const c2) -> Const(c1-c2)
    Oper Times c@(Const 0) y -> c
    Oper Times x c@(Const 0) -> c
    Oper Times (Const 1) c@(Const c1) -> c
    Oper Times c@(Const c1) (Const 1) -> c
    Oper Times (Const c1) (Const c2) -> Const(c1*c2)
    Oper op e1 e2 -> Oper op (simplify e1) (simplify e2)
    Let v e body ->
      if body `containsVar` v then
        Let v (simplify e) (simplify body)
      else
        simplify body
    _ -> e

containsVar :: Expr -> Ident -> Bool
containsVar (Var v) i = v == i
containsVar (Const n) i = False
containsVar (Oper _ x y) i = x `containsVar` i || y `containsVar` i
containsVar (Let _ _ body) i = body `containsVar` i 