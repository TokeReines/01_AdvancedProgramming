-- 1. Touring Haskell
-- 1.1 Finish the declaration of the function move
type Pos = (Int, Int)
data Direction = North | South | East | West 
  deriving (Show)

-- move North (1,1) or 
-- myPos = (1,1)
-- move North myPos
move :: Direction -> Pos -> Pos 
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East  (x,y) = (x+1,y)
move West  (x,y) = (x-1,y)

-- 1.2 Declare a function moves with the type
-- myMoves = [North, South]
-- myPos = (1,1)
moves :: [Direction] -> Pos -> Pos
moves [] _ = (0,0)
moves [dir] pos = move dir pos
moves (dir:dirs) pos = moves dirs (move dir pos)  

-- 1.3 Declare functions for adding and multiplying natural numbers, using the 
-- following data type declaration:
data Nat = Zero | Succ Nat
    deriving (Eq, Show, Read, Ord) 

createNat :: Int -> Nat
createNat x
  | x < 0 = Zero
  | x == 0 = Zero
  | x == 1 = Succ Zero
  | otherwise = Succ (createNat (x-1))

addNat :: Nat -> Nat -> Nat
addNat Zero succ = succ -- Left is Zero
addNat succ Zero = succ -- Right is Zero
addNat x (Succ y) = addNat (Succ x) y 

addNatTest = 
  addNat (createNat 0) (createNat 0) == Zero &&
  addNat (createNat 0) (createNat 1) == createNat 1 &&
  addNat (createNat 1) (createNat 0) == createNat 1 &&
  addNat (createNat 1) (createNat 1) == createNat 2 &&
  addNat (createNat 2) (createNat 1) == createNat 3 &&
  addNat (createNat 1) (createNat 2) == createNat 3 &&
  addNat (createNat 2) (createNat 2) == createNat 4 &&
  addNat (createNat 3) (createNat 1) == createNat 4 &&
  addNat (createNat 1) (createNat 3) == createNat 4 &&
  addNat (createNat 7) (createNat 9) == createNat 16

mulNat :: Nat -> Nat -> Nat
mulNat _ Zero = Zero
mulNat Zero _ = Zero
mulNat x (Succ Zero) = x
mulNat x (Succ y) = addNat x (mulNat x y)


mulNatTest = 
  mulNat (createNat 0) (createNat 0) == createNat 0 &&
  mulNat (createNat 0) (createNat 1) == createNat 0 &&
  mulNat (createNat 1) (createNat 0) == createNat 0 &&
  mulNat (createNat 1) (createNat 1) == createNat 1 &&
  mulNat (createNat 2) (createNat 1) == createNat 2 &&
  mulNat (createNat 1) (createNat 2) == createNat 2 &&
  mulNat (createNat 2) (createNat 2) == createNat 4 &&
  mulNat (createNat 3) (createNat 1) == createNat 3 &&
  mulNat (createNat 1) (createNat 3) == createNat 3 &&
  mulNat (createNat 2) (createNat 3) == createNat 6 &&
  mulNat (createNat 3) (createNat 2) == createNat 6 &&
  mulNat (createNat 3) (createNat 3) == createNat 9 &&
  mulNat (createNat 5) (createNat 5) == createNat 25 &&
  mulNat (createNat 7) (createNat 9) == createNat 63

-- 1.4 Declare functions nat2int and int2nat to go from natural numbers (the 
-- above data type) to Haskell integers and back.

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ Zero) = 1
nat2int (Succ x) = 1 + nat2int x 

int2nat :: Int -> Nat
int2nat = createNat

-- 1.5 Given the data type for representing binary search trees with integers stored in the nodes:
-- Declare a function insert that takes an integer n and a binary search tree t, and returns a
-- new search tree t' with n inserted correctly into t. If n was already present in t, the 
-- function should just return t as t'. Insert new elements at the leaves only; do not worry 
-- about keeping the tree balanced.

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

-- All leaves in left/right subtree are less/greater than node value
insert :: Int -> Tree -> Tree
insert x Leaf = Node x Leaf Leaf
insert x (Node v t1 t2)
  | v == x = Node v t1 t2
  | v < x  = Node v t1 (insert x t2)
  | otherwise  = Node v (insert x t1) t2

-- 1.6.1 Change the declaration of Tree so that it is polymorphic in the data stored in the nodes.
data Tree' a = Leaf' | Node' a (Tree' a) (Tree' a)
  deriving (Eq, Show, Read, Ord)

insert' :: (Ord a) => a -> Tree' a -> Tree' a
insert' x Leaf' = Node' x Leaf' Leaf'
insert' x (Node' v t1 t2)
  | v == x = Node' v t1 t2
  | v < x  = Node' v t1 (insert' x t2)
  | otherwise  = Node' v (insert' x t1) t2

-- 1.6.2 What happens to the type of the function insert?
-- Goes from: insert :: Int -> Tree -> Tree
-- To       : Ord a => a -> Tree' a -> Tree' a

-- 2. Morse code
-- https://gist.github.com/saclark/fc0c1b3ee09810826b79


latin2Morse :: [(Char, String)]
latin2Morse = [('A', ".-"), ('N', "-."), ('B', "-..."), ('O', "---"), ('C', "-.-."), ('P', ".--."), ('D', "-.."), ('Q', "--.-"),('E', "."), ('R',  ".-."),('F', "..-."), ('S', "..."),('G', "--."), ('T', "-"),('H', "...."), ('U', "..-"),  ('I', ".."), ('V', "...-"),  ('J', ".---"), ('W', ".--"),  ('K', "-.-"), ('X', "-..-"),  ('L', ".-.."), ('Y', "-.--"),  ('M', "--"), ('Z', "--..")]

morse2Latin :: [(String, Char)]
morse2Latin = [(".-", 'A'),("-.", 'N'),("-...", 'B'),("---", 'O'),("-.-.", 'C'),(".--.", 'P'),("-..", 'D'),("--.-", 'Q'),(".", 'E'),( ".-.", 'R'),("..-.", 'F'),("...", 'S'),("--.", 'G'),("-", 'T'),("....", 'H'),("..-", 'U'),("..", 'I'),("...-", 'V'),(".---", 'J'),(".--", 'W'),("-.-", 'K'),("-..-", 'X'),(".-..", 'L'),("-.--", 'Y'),("--", 'M'),("--..", 'Z')]

-- decode :: String -> [String]
-- decode "" = [""]
-- decode (x:xs) = lookup [x] morse2Latin

-- decode :: String -> [String]
-- decode [] = [""]
-- decode x =
--     [ c : dtail | (c, m) <- code, isPrefixOf m x, dtail <- decode (drop (length m) x) ]


-- 4. Type classes
-- Following is an interface for types that can be said to have a size
class Sizeable t where
  size :: t -> Int

instance Sizeable Int where
  size _ = 1