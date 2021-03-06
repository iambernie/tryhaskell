{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdArgs
import Data.Tree
import System.IO

data Saw = Saw {nrconfigs :: Int
               ,yrname :: String}
              deriving (Show, Data, Typeable)

sawArgs = Saw {nrconfigs = def, yrname =""}

ptree :: (Show a) => Tree a -> IO ()
ptree = putStrLn . drawTree . stringTree
      where  stringTree :: (Show a) => Tree a -> Tree String
             stringTree = fmap show

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

root :: Tree (Punt, Path)
root = Node ((0,1), [(0,0)]) []

allowedMoves :: Path -> Punt -> [Punt]
allowedMoves path pt =  filter (`notElem` path) [move pt | move <- moves]

insertMoves :: Tree (Punt, Path) -> Tree (Punt, Path)
insertMoves (Node (pos, path) []) = Node (pos, path) trees
    where trees = [Node (p, pos:path) [] | p <- allowedMoves path pos ]
insertMoves (Node (pos, path) trees) = Node (pos, path) (map insertMoves trees)

nTree :: Int -> Tree (Punt, Path)
nTree 0 = root
nTree n = iterate insertMoves (nTree 0) !! (n-1)

--fpow n = foldr (.) id . replicate n

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

main :: IO ()
main =  cmdArgs sawArgs >>= \opts ->
        putStr "Hi " >> putStrLn (yrname opts) >>
        putStrLn ("Number of configurations to run: "++ (show $ nrconfigs opts) )>>
        --putStrLn (show $ reSqrd (nrconfigs opts)) >>
        putStrLn (show $ map zsaw [1..(nrconfigs opts)]) >>
        putStrLn (show $ map sumSqrdDistances [1..(nrconfigs opts)]) >>

        return ()
        --write to file

