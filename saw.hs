import Data.Tree

ptree :: (Show a) => Tree a -> IO ()
ptree = putStrLn . drawTree . stringTree
      where  stringTree :: (Show a) => Tree a -> Tree String
             stringTree = fmap show

listpaths :: Tree a -> [[a]]
listpaths (Node label []) = [[label]]
listpaths (Node label xs) = map (label:) $ concat $ map listpaths xs

listendpoints :: Tree a -> [a]
listendpoints (Node label []) = [label]
listendpoints (Node label trees) = concat $ map listendpoints trees

type Punt = (Int, Int)
type Path = [Punt]

data Direction = North | South | West | East deriving Show

move :: Direction -> Punt -> Punt
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move West  (x,y) = (x-1, y)
move East  (x,y) = (x+1, y)

moves :: [Punt -> Punt]
moves = [move North, move South, move West, move East]

origin = (0,0) :: Punt

root :: Tree (Punt, Path)
root = Node (origin, []) []

sqrdDistance :: Punt -> Int
sqrdDistance (x,y) = x^2+y^2

allowedMoves :: Path -> Punt -> [Punt]
allowedMoves path pt =  filter (`notElem` path) [move pt | move <- moves]

insertMoves :: Tree (Punt, Path) -> Tree (Punt, Path)
insertMoves (Node (pos, path) []) = Node (pos, path) [Node (p, pos:path) [] | p <- allowedMoves path pos ]
insertMoves (Node (pos, path) trees) = Node (pos, path) (map insertMoves trees)

nTree :: Int -> Tree (Punt, Path)
nTree 0 = root
nTree n = iterate insertMoves (nTree 0) !! n

fpow n = foldr (.) id . replicate n




