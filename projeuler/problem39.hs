{-
If p is the perimeter of a right angle triangle with integral length sides {a,b,c}, 
there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p <= 1000, is the number of solutions maximised?
-}

import Data.List (sort, nub)

combs :: Int -> [[Int]]
combs p = do
    a <- [1..p]
    b <- [1..p]
    c <- [1..p]
    return [a,b,c]

combs' :: Int -> [[Int]]
combs' p = [[x,y,z] | x<-[1..p], y<-[1..p], z<-[1..p]]

isPythagoreanTriple :: [Int] -> Bool
isPythagoreanTriple (a:b:c:_) | a^2 + b^2 == c^2  = True
                              | otherwise         = False

triangles :: Int -> [[Int]]
triangles p = map sort $ filter (\c -> sum c == p) (combs' p)

solutions :: Int -> [[Int]]
solutions p = nub $ filter isPythagoreanTriple $ triangles p

maximised :: Int

main =  print (solutions 120) >>
        print (map length $ map solutions [1..1000]) >>
        return ()
