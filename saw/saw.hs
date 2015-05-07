{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdArgs
import Data.Tree

data Saw = Saw {configurations :: String}
              deriving (Show, Data, Typeable)

sawArgs = Saw {configurations = def}

data PolyTree a = End
                | Branch a (PolyTree a) (PolyTree a) (PolyTree a) (PolyTree a)
                deriving (Show)


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
root = Node ((0,1), [(0,0)]) []

sqrdDistance :: Punt -> Int
sqrdDistance (x,y) = x^2+y^2

allowedMoves :: Path -> Punt -> [Punt]
allowedMoves path pt =  filter (`notElem` path) [move pt | move <- moves]

insertMoves :: Tree (Punt, Path) -> Tree (Punt, Path)
insertMoves (Node (pos, path) []) = Node (pos, path) trees
    where trees = [Node (p, pos:path) [] | p <- allowedMoves path pos ]
insertMoves (Node (pos, path) trees) = Node (pos, path) (map insertMoves trees)

nTree :: Int -> Tree (Punt, Path)
nTree 0 = root
nTree n = iterate insertMoves (nTree 0) !! (n-1)

-- evt trees = iterate insertMoves (nTree 0)

fpow n = foldr (.) id . replicate n

paths :: [(Punt, Path)] -> [Path]
paths ((punt, path):[])  = [path]
paths ((punt, path):xs)  = path:[] ++ paths xs


pathlengths :: Tree (Punt, Path) -> [Int]
pathlengths tree = map length $ (paths . flatten) tree

zsaw :: Int -> Int
zsaw n = length $ filter(==(n)) $ pathlengths (nTree (n))

sumSqrdDistances :: Int -> Int
sumSqrdDistances n = sum $ map (\((x,y),path) -> x^2+y^2) $ filter (isLength n) (flatten $ nTree n)

isLength :: Int -> (Punt, Path) -> Bool
isLength n (punt, path) = length path == n

reSqrd :: Int -> Float
reSqrd n = fromIntegral (sumSqrdDistances n) / fromIntegral (zsaw n)


main =  cmdArgs sawArgs >>= print
     -- run program

     -- writetofile
     --print (map zsaw [1..10]) >>
     --print "done"

