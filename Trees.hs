{-
 -  Tree a with a :: Char


                        Branch
                         /  \
                        /    \
                   Branch    Leaf 'a'
                   /   \
                  /     \
              Branch    Leaf 'b'
               /  \
              /    \
        Leaf 'c'    Leaf 'd'
-}

myabcdTree = Branch (Branch
                           (Branch
                                   (Leaf 'c')
                                   (Leaf 'd'))
                           (Leaf 'b'))
                    (Leaf 'a')

mynumberTree = Branch (Branch (Branch (Leaf 3) (Leaf 4)) (Leaf 2)) (Leaf 1)

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Branch left right) = Branch (treeMap f left) (treeMap f right)

showTree :: (Show a) => Tree a -> String
showTree (Leaf x) = show x
showTree (Branch l r) = "<"++ showTree l ++"|"++ showTree r ++">"

showsTree :: (Show a) => Tree a -> String -> String
showsTree (Leaf x) = shows x
showsTree (Branch l r) = ('<':) . showsTree l . ('|':) . showsTree r . ('>':)

readsTree :: (Read a) => ReadS (Tree a)
--readsTree :: (Read a) => String -> [(Tree a, String)]
readsTree ('<':s) = [(Branch l r, u) | (l, '|':t) <- readsTree s,
                                       (r, '>':u) <- readsTree t ]
readsTree s       = [(Leaf x, t)    | (x,t) <- reads s]


{-
   eenBoom:


                               Tak 4
                             /       \
                            /         \
                           /           \
                      Tak 2             Tak 6
                     /     \           /     \
                    /       \         /       \
               Tak 1       Blad    Blad      Tak 7
              /     \                        /     \
             /       \                      /       \
         Blad        Blad                Blad       Blad
 -}

data Boom a = Tak a (Boom a) (Boom a)
            | Blad
            deriving (Show)

eenBoom = Tak 4 (Tak 2 (Tak 1 Blad Blad)
                       (Blad))
                (Tak 6 (Blad)
                       (Tak 7 Blad Blad))


omvang :: Boom a -> Int
omvang Blad        = 0
omvang (Tak _ p q) = 1 + omvang p + omvang q

telBlad :: Boom a -> Int
telBlad Blad        = 1
telBlad (Tak _ p q) = telBlad p + telBlad q

{-

    Prelude> insertBoom 5 Blad
    Tak 5 Blad Blad
    it :: (Ord a, Num a) => Boom a

                      Tak 5
                     /     \
                    /       \
                 Blad       Blad

    Prelude> insertBoom 200 $ insertBoom 5 Blad
    Tak 5 Blad (Tak 200 Blad Blad)

                     Tak 5
                    /     \
                   /       \
                Blad       Tak 200
                           / \
                          /   \
                       Blad   Blad

    Prelude> insertBoom 4 $ insertBoom 200 $ insertBoom 5 Blad
    Tak 5 (Tak 4 Blad Blad) (Tak 200 Blad Blad)

                    Tak 5
                   /     \
                  /       \
               Tak 4       Tak 200
                / \         / \
               /   \       /   \
           Blad   Blad  Blad   Blad

    Prelude> insertBoom 6 $ insertBoom 4 $ insertBoom 200 $ insertBoom 5 Blad
    Tak 5 (Tak 4 Blad Blad) (Tak 200 (Tak 6 Blad Blad) Blad)


                      Tak 5
                     /     \
                    /       \
                 Tak 4       Tak 200
                  / \         / \
                 /   \       /   \
             Blad   Blad  Tak 6   Blad
                           / \
                          /   \
                        Blad Blad


    Prelude> lijstNaarBoom [6,4,200,5]
    Tak 5 (Tak 4 Blad Blad) (Tak 200 (Tak 6 Blad Blad) Blad)

    Prelude> insertLijstInBoom (reverse [6,4,200,5]) Blad
    Tak 5 (Tak 4 Blad Blad) (Tak 200 (Tak 6 Blad Blad) Blad)

-}

insertBoom :: Ord a => a -> Boom a -> Boom a
insertBoom e Blad = Tak e Blad Blad
insertBoom e (Tak x links rechts) | e <= x  = Tak x (insertBoom e links) rechts
                                  | e  > x  = Tak x links (insertBoom e rechts)

insertLijstInBoom :: Ord a => [a] -> Boom a -> Boom a
insertLijstInBoom [] Blad                     = Blad
insertLijstInBoom (x:xs) Blad                 = insertLijstInBoom xs (Tak x Blad Blad)
insertLijstInBoom [] (Tak p links rechts)     = Tak p links rechts
insertLijstInBoom (x:xs) (Tak p links rechts) = insertLijstInBoom xs (insertBoom x (Tak p links rechts))

lijstNaarBoom :: Ord a => [a] -> Boom a
lijstNaarBoom = foldr insertBoom Blad

labels :: Boom a -> [a]
labels Blad                 = []
labels (Tak p links rechts) = labels links ++ [p] ++ labels rechts

sorteer :: Ord a => [a] -> [a]
sorteer = labels . lijstNaarBoom

--listpaths :: Tree a -> [[a]]
--listpaths (Node label []) = [[label]]
--listpaths (Node label xs) = map (label:) $ concat $ map listpaths xs




