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
add Zero y = y
add x Zero = x
add x (Succ y) = add (Succ x) y

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult Zero _ = Zero
mult x (Succ Zero) = x
mult x (Succ y) = add x (mult x y)

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = 1 + nat2int x

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ (int2nat (x - 1))

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert x Leaf = Node x Leaf Leaf
insert x (Node n left right)
  | x > n = Node n left (insert x right)
  | x == n = Node n left right
  | otherwise = Node n (insert x left) right

-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)

--pinsert :: FIXME  -- uncomment and replace with the proper type of pinsert
pinsert :: (Eq a, Ord a) => a -> PTree a -> PTree a
pinsert x PLeaf = PNode x PLeaf PLeaf
pinsert x (PNode n left right)  
    | x > n = PNode n left (pinsert x right)
    | x == n = PNode n left right
    | otherwise = PNode n (pinsert x left) right