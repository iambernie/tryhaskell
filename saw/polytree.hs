{-# LANGUAGE DeriveDataTypeable #-}

import Data.Tree
import System.Environment
import System.Exit

parse :: [String] -> IO String
parse ["-h"] = usage   >> exit
parse []     = getContents
--parse fs     = concat `fmap` mapM readFile fs
usage   = putStrLn "Usage: polytree [-vh] [file ..]"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)


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

data PolyTree a = NotAllowed
                | Branch a (PolyTree a) (PolyTree a) (PolyTree a) (PolyTree a)
                deriving (Show)

instance Functor PolyTree where
  fmap f NotAllowed                    = NotAllowed
  fmap f (Branch x u d l r) = Branch (f x) (fmap f u) (fmap f d) (fmap f l) (fmap f r)

-- instance Monad PolyTree where
--   return     = 
--   (>>=)      =
-- 
-- instance Applicative PolyTree where
--   pure       =
--   <*>        =
-- 
-- instance Traversable PolyTree where
--   traverse   =



{-
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

instance Functor Tree where
  fmap f Empty        = Empty
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Node l k r) = Node (fmap f l) (f k) (fmap f r)
-}

main = print "Running polytree.hs..." >>
       getArgs >>= parse              >>
       print "Building Tree.." 
     -- run program


























