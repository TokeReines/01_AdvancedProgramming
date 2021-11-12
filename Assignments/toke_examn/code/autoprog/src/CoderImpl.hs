-- Put your Coder implementation in this file
module CoderImpl where

import Defs
import Control.Monad (ap, liftM)

-- no need to touch these
instance Functor Tree where fmap = liftM
instance Applicative Tree where pure = return; (<*>) = ap

instance Monad Tree where
  return x        = Found x
  Found x >>= f   = f x
  Choice xs >>= f = Choice (map (>>= f) xs)

pick :: [a] -> Tree a
pick as = Choice $ map Found as

solutions :: Tree a -> Int -> Maybe a -> [a]
solutions tr x (Just a) = [a]
solutions tr 0 Nothing = []
solutions tr n d = case tr of
  Found a -> [a] -- note rename stuff
  Choice [] -> []
  Choice (x:xs) -> case x of
    Found a -> a : solutions (Choice xs) (n-1) d
    Choice xs' -> solutions (Choice (xs ++ xs')) (n -1) d

produce :: [(String,SType)] -> SType -> Tree Exp
produce = undefined

-- recommended, but not mandated, helper function:
extract :: [(String,SType)] -> SType -> SType -> Tree (Exp -> Exp)
extract = undefined
