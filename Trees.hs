
{-
 -  Tree a with a :: Char


                               Branch
                             /        \
                            /          \
                      Branch             Leaf 'a'
                     /     \
                    /       \
              Branch         Leaf 'b'
             /      \
            /        \
      Leaf 'c'        Leaf 'd'

 -}

myabcdTree = Branch
                    (Branch
                           (Branch
                                   (Leaf 'c')
                                   (Leaf 'd'))
                           (Leaf 'b'))
                    (Leaf 'a')

mynumberTree = Branch (Branch (Branch (Leaf 3) (Leaf 4)) (Leaf 2)) (Leaf 1)

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving (Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Branch left right) = Branch (treeMap f left) (treeMap f right)

--leafCount :: Tree a -> Int
--leafCount
--
--treeSum :: Tree a -> Int






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

--telTakwaarden :: Boom a -> Int
--telTakwaarden Blad        = 0
--telTakwaarden (Tak x p q) = x + (telTakwaarden p) + (telTakwaarden q)

type Naam = String
data StamBoom = Vader Naam
              | Moeder Naam


-- Zoeken in lineaire data structuur:
--
--    Lege Lijst:
--    =======================================================
--    | | | | | | | | | | | | | | | | | | | | | | | | | | | |
--    =======================================================
--
--    voegToe 1 Lijst
--    voegToe 9 Lijst
--    voegToe 7 Lijst
--    voegToe 3 Lijst
--    voegToe 5 Lijst
--
--    =======================================================
--    |5|3|7|9|1| | | | | | | | | | | | | | | | | | | | | | |
--    =======================================================
--
--
--
-- Zoekbomen
--
-- een zoekboom opbouwen:
--
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








