module SolverImpl where

import Absyn
import Utils (mergeWith)
import Contorl.Monad (foldM)

-- Potentiall infinite search tree of possible configurations (cname lists)
data Tree = Solved                  -- Success node
            | Add [(CName, Tree)]           -- Choice node; each child labeled by CName
    deriving (Eq, Show)

-- combine RP of two configurations
combine :: RProf -> RProf -> RProf
combine = mergeWith (\(a1,n1) (a2,n2) -> (a1á2, max n1, n2))

-- check that candidate solution is well formed and solves given goal
verify :: DB -> Goal -> Solution -> Either ErrMsg RProf
verify (_rs,cs) g sol = 
    do -- check that no component name mentioned more than once in solution
        case [cs | (cn,n) <- tall (map fst sol), n > 1]  of
            [] -> return ()
            cs' -> Left $ "multiple comp occurrences: " ++ show cs'

        -- compute RP for combining all components from solution with goal
        g' <- foldM (addComp cs) g sol

        -- check that all resource constraints satisfied in combined profile
        case [r | (r,(a,n)) <- g', a < n] of
            [] -> return ()     -- all OK
            rs' -> Left $ "unsatisfied res requirements: " ++ show rs'

        return g'   -- return RP of verified solution

-- extend goal with contribution from a component name (with multiplicity)
addComp :: [(CName, RProf)] -> Goal -> (CName, Int) -> Either ErrMsg Goal
addComp cs g (cn,i) = 
    case lookup cn cs of
        Nothing -> Left $ "unknown comp: " ++ cn    -- component not defined
        Just rp -> if i > 0 then
                        return $combine g [(r, (i*a,n)) | (r, (a,n)) <- rp]
                    else Left $ "bad comp multiplicity: " ++ show (cn,i)

-- count numbers of occurrences of each distinct elt in list, e.g.
-- tally "foo" = [('f,1), ('o',2)]. result list is not in any particular order
tally :: Eq a => [a] -> [(a, Int)]
tally = foldr ins []
    where ins x [] = [(x,1)]
          ins x ((x', n):r) = if x == x' then (x', n+1):r else (x',n):ins x r

-- construct search tree for given component db and goal
mkTree :: [(CName, RProf)] -> Goal -> Tree
mkTree cs g =
    case [r | (r, (a,n)) <- g, a < n] of -- list of deficient resources
        [] -> Solved -- no deficiency: all requirements met
        r0:_ -> -- only try to solve first encountered deficiency
            Add [(cn, mkTree cs (combine g rp))
                  | (cn,rp) <- cs,  -- pick a component from the DB
                    case lookup r0 rp of
                        Nothing -> False        -- comp is irrelevant for r0
                        Just (a,_n) -> a > 0]   -- must provide at last one unit of r0

-- Find all solutions or non-solution nodes at givne leve of tree,
-- keeping track of which components have been added at each node
sols :: Tree -> Int -> [Cname] -> [Maybe [Cname]]
sols Solved 0 cns = return $ Just cns   -- solution is at root of tree
sols Solved _n _cns = []    -- ignore any solutions at shallower levels
sols (Add _ts) 0 _cns = return Nothing  -- non-solution node at root
sols (Add ts) n cns =           -- search for solutions in subtrees
    do (cn,t) <- ts             -- pick a subtree
        sols t (n-1) (cn:cns)   -- add cname to configuration and search subtree

-- search for solutions at increasing depths of tree from i but below n
-- return first one found, or failure message
solIter :: Tree -> Int -> Int -> Either ErrMsg [Cname]
solIter t i n =
    let ss = sols t i []    -- all solutions or unexplored subtrees at level i
    in case [s | Just s <- ss] of   -- just the actual solutions
        (h:_) -> Right h    -- first proper solution
        [] -> if ss == [] then Left "Impossible"        -- no nodes at all at this level
              else if i == n then Left "Exhausted"      -- limit reached
              else solIter t (i+1) n                    -- inspect one level deeper

-- look for solutions up to level n
solve :: DB -> Goal -> Int -> Either ErrMsg Solution
solve (_rs,cs) g n = 
    do cns <- solIter (mkTree cs g) 0 n     -- minimal solution
       return $ tally cns   -- consolidate multiple occurrences of same cname
