-- Task 1
type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move East  (x,y) = (x+1, y)
move South  (x,y) = (x, y-1)

moves :: [Direction] -> Pos -> Pos 
moves [] (x, y) = (x, y)
moves (d:ds) (x, y) = moves ds (move d (x, y))

-- Task 2
data Nat = Zero | Succ Nat
    deriving (Eq, Show, Read, Ord) 

natAdd :: Nat -> Nat -> Nat
natAdd Zero Zero = Zero
natAdd (Succ x) Zero = Succ (natAdd x Zero)
natAdd Zero (Succ y) = Succ (natAdd Zero y)
natAdd (Succ x) (Succ y) = Succ(Succ(natAdd x y))

nat2Int :: Nat -> Integer
nat2Int Zero = 0
nat2Int (Succ x) = 1 + nat2Int x

int2Nat :: Integer -> Nat
int2Nat 0 = Zero
int2Nat x = Succ (int2Nat (x - 1))

-- Task 3
-- All leaves in left/right subtree are less/greater than node value
data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Eq, Show, Read, Ord)

insert :: Tree Int -> Int -> Tree Int
insert Leaf x = Node x Leaf Leaf
insert (Node n left right) x  
    | x > n = Node n left (insert right x)
    | x == n = Node n left right
    | otherwise = Node n (insert left x) right