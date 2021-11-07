-- Put your Resolver implementation in this file
module ResolverImpl where

import Defs
-- tce0 :: TCEnv
-- tce0 =
--   [("(,)", \ts -> case ts of [t1,t2] -> return $ STProd t1 t2
--                              _ -> Left "bad args for (,)"),
--    ("(->)", \ts -> case ts of [t1,t2] -> return $ STArrow t1 t2
--                               _ -> Left "bad args for (->)")]
-- PTApp TCName [PType]
 -- resolve tce0 (\x -> return $ STVar (x++"'")) PTApp "(->)" [PTVar "a", PTVar "a"] @?= Right STArrow (STVar "a'") (STVar "a'"),
resolve :: TCEnv -> (TVName -> EM SType) -> PType -> EM SType
resolve tce tve (PTApp tcn [t1, t2]) = case lookup tcn tce of
                      Just f -> do
                          x1 <- tve "t1"
                          x2 <- tve "t2"
                          case f [x1, x2] of
                              Right x -> Right x
                              Left e -> Left e
                      Nothing -> Left ""
resolveTCE :: TCEnv -> TCName -> EM SType
resolveTCE tce = undefined
    -- case lookup tcn tce of
    --     Just f -> 


declare :: [TDecl] -> EM TCEnv
declare = undefined
