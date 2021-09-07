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