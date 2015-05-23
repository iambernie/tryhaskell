{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdArgs
import System.IO
import Data.List (foldl')
import Control.Parallel

data Saw = Saw {nrconfigs :: Int
               }
              deriving (Show, Data, Typeable)

sawArgs = Saw {nrconfigs = def}


data Boom a = Eind
            | Pad a
            | Tak (Boom a) (Boom a) (Boom a) (Boom a)  deriving (Show)

instance Functor Boom where
  fmap f Eind          = Eind
  fmap f (Pad ps)      = Pad (f ps)
  fmap f (Tak u d l r) = Tak (fmap f u) (fmap f d) (fmap f l) (fmap f r)

type Punt = (Int, Int)

insertM :: Boom [Punt] -> Boom [Punt]
insertM Eind = Eind
insertM (Pad ps)  = Tak (goUp ps) (goDown ps) (goLeft ps) (goRight ps)
insertM (Tak u d l r) = Tak (insertM u) (insertM d) (insertM l) (insertM r)

goUp :: [Punt] -> Boom [Punt]
goUp ((x,y):ps) | p `notElem` ps = Pad (p:(x,y):ps)
                | otherwise      = Eind
                where p = (x, y+1)

goDown :: [Punt] -> Boom [Punt]
goDown ((x,y):ps) | p `notElem` ps = Pad (p:(x,y):ps)
                  | otherwise      = Eind
                  where p = (x, y-1)


goLeft :: [Punt] -> Boom [Punt]
goLeft ((x,y):ps) | p `notElem` ps = Pad (p:(x,y):ps)
                  | otherwise      = Eind
                  where p = (x-1, y)

goRight :: [Punt] -> Boom [Punt]
goRight ((x,y):ps) | p `notElem` ps = Pad (p:(x,y):ps)
                   | otherwise      = Eind
                   where p = (x+1, y)


root :: Boom [Punt]
root = Tak (Pad [(0,1),(0,0)]) Eind Eind Eind

eindPunten :: Boom [Punt] -> [Punt]
eindPunten Eind = []
eindPunten (Pad (p:ps)) = [p]
eindPunten (Tak u d l r) = concat [(eindPunten u), (eindPunten d), (eindPunten l), (eindPunten r)]

afstandKwadraat :: Punt -> Int
afstandKwadraat (x,y) = x^2 + y^2

sumSqrdDist :: [Punt] -> Int
sumSqrdDist = foldl' (\acc (x,y) -> x^2+y^2+acc) 0

zsaw :: [Punt] -> Int
zsaw = foldl' (\acc _ -> acc+1) 0


nBoom n = iterate insertM root !! n

--nBoom :: Int -> Boom [Punt]
--nBoom = ((iterate insertM (nBoom' 0))!!)
--      where nBoom' 0 = root
--            nBoom' n = nBoom (n-1)

main :: IO ()
main =  cmdArgs sawArgs >>= \opts ->
        putStrLn ("Number of configurations to run: "++ (show $ nrconfigs opts) )>>
        putStrLn (show $ map (\xs -> (zsaw xs)) $ map eindPunten $ map nBoom [0..(nrconfigs opts)]) >>
        putStrLn (show $ map (\xs -> (sumSqrdDist xs)) $ map eindPunten $ map nBoom [0..(nrconfigs opts)]) >>
        return ()

