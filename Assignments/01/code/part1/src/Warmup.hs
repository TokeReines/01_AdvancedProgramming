module Warmup where

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move South  (x,y) = (x, y-1)
move East  (x,y) = (x+1, y)
move West  (x,y) = (x-1, y)

moves :: [Direction] -> Pos -> Pos
moves [] p = p
moves [d] p = move d p -- Check if this functions is neccesary
moves (d:ds) p = moves ds (move d p)

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add = undefined

mult :: Nat -> Nat -> Nat
mult = undefined

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int = undefined

int2nat :: Int -> Nat
int2nat = undefined

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert = undefined

-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)

--pinsert :: FIXME  -- uncomment and replace with the proper type of pinsert
pinsert = undefined
