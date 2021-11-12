-- Put your Resolver implementation in this file
module ResolverImpl where

import Defs
import qualified Data.Map as Map
-- tce0 :: TCEnv
-- tce0 =
--   [("(,)", \ts -> case ts of [t1,t2] -> return $ STProd t1 t2
--                              _ -> Left "bad args for (,)"),
--    ("(->)", \ts -> case ts of [t1,t2] -> return $ STArrow t1 t2
--                               _ -> Left "bad args for (->)")]
-- PTApp TCName [PType]
 -- resolve tce0 (\x -> return $ STVar (x++"'")) PTApp "(->)" [PTVar "a", PTVar "a"] @?= Right STArrow (STVar "a'") (STVar "a'"),
resolve :: TCEnv -> (TVName -> EM SType) -> PType -> EM SType
resolve tce tve (PTVar x) = tve x
resolve tce tve (PTApp tcn [t1, t2]) = do  
    l <- resolve tce tve t1
    r <- resolve tce tve t2
    Map.fromList tce0 Map.! tcn $ [l ,r]


resolveTCE :: TCEnv -> TCName -> EM SType
resolveTCE tce = undefined

declare :: [TDecl] -> EM TCEnv
declare = undefined
