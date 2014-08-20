-- Recommended style of importing.
-- YES:
-- import qualified Very.Special.Module as VSM
-- import Another.Important.Module (printf, (<|>), )
--
-- NO:
-- import Very.Special.Module
-- import Another.Important.Module hiding (open, close, )
--
-- Nomenclature: the following "reserved words" have a special meaning for the
--               interpreter:
--
--               case of
--
--               if then else
--
--               let where
--
--               infix infixl infixr
--
--               class instance
--
--               data type
--
--               primitive
--               in
--
--
--
--
--               GET DIRTY QUICK ---> ghci>
--
--
-- Lambda-calculus: a mathematical formalism in which the key concepts are
--                  the <expression>, the <function> and the <application>.
--                  They are defined as follows:
--
--                  <expression>  := <name> | <function> | <application>
--                  <function>    := lambda <name> . <expression>
--                  <application> := <expression> <expression>
--
--                  ....
--
--                  it boils down to this:
--
--                     Math notation           ||  Haskell Code
--                     --------------------------------
-- Named Function      f(x, y) = x^2 + y^2     ||  f x y = x^2 + y^2
-- Lambda abstraction  lambda xy . x^2 + y^2   ||  \x y -> x^2 + y^2
--
--                  So for instance, the lambda abstraction could for example
--                  be used like this:
--
--                  Prelude> map (\x -> 3 * x) [1..10]
--                  [3,6,9,12,15,18,21,24,27,30]
--
--                  which uses a lambda abstraction, but is quite a bit of typing.
--
--                  Or consider this:
--                  Prelude> let foo x = 3*x
--                  Prelude> map foo [1..10]
--                  [3,6,9,12,15,18,21,24,27,30]
--
--                  Or simply:
--                  Prelude> map (3*) [1..10]
--                  [3,6,9,12,15,18,21,24,27,30]
--
--
-- Syntax:
--
-- Declaration of a function.
--
-- <func> <arg>
--
-- Declaration of a
--
-- Style:
--

import Data.List (union, sort)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fib n = fibs !! n

qsort []     = []
qsort (x:xs) = qsort [y | y <- xs, y < x ] ++ [x] ++
               qsort [y | y <- xs, y >= x]

raise' x ys = map (x+) ys
raise x = map (x+)


fac n = product [1..n]

fac' 0 = 1
fac' n = n * fac (n-1)

fac'' = scanl (*) 1 [2..]


boven n k = fac n / (fac k * fac (n-k))

plus :: Int -> Int -> Int
plus a b = a + b

opvolger :: Int -> Int
opvolger = plus 1

-- operator-secties
verdubbel :: Num a => a -> a
verdubbel = (2*)

halveer :: Fractional a => a -> a
halveer = (/2.0)

omgekeerde :: Fractional a => a -> a
omgekeerde = (1.0/)

kwadraat :: Integral a => a -> a
kwadraat = (^2)

tweeTotDe :: Integral a => a -> a
tweeTotDe = (2^)

eencijferig :: Integral a => a -> Bool
eencijferig = (<=9)

isNul :: Integral a => a -> Bool
isNul = (==0)

-- map
map' :: (a->b) -> [a] -> [b]
map' functie [] = []
map' functie (x:xs) = functie x : map' functie xs


-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' predikaat []                     = []
filter' predikaat (x:xs) | predikaat x   = x : filter predikaat xs
                         | otherwise     = filter predikaat xs


lengte :: [a] -> Int
lengte [] = 0
lengte (kop:staart) = 1 + lengte staart


sum' :: Num a => [a] -> a
sum' []         = 0
sum' (x:xs)     = x + sum' xs

--type sig?
sum'' []        = foldr' (+) 0

product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

--type sig?
product'' []        = foldr' (*) 1

and' :: [Bool] -> Bool
and' []         = True
and' (x:xs)     = x && and xs

--type sig?
and'' []        = foldr' (&&) True


oneven :: Integral a => a -> Bool
oneven x = not (even x)


foldr' :: (a->b->b) -> b -> [a] -> b
foldr' oper expr []      = expr
foldr' oper expr (x:xs)  = x `oper` foldr' oper expr xs

foldl' :: (b->a->b) -> b -> [a] -> b
foldl' oper expr [] = expr
foldl' oper expr (x:xs) = foldl oper (expr `oper` x) xs

until' :: (a->Bool) -> (a->a) -> a -> a
until' p f x  | p x       = x
              | otherwise = until' p f (f x)

elem' :: Eq a => a -> [a] -> Bool
elem' e xs = or (map (==e) xs)

elem'' :: Eq a => a -> [a] -> Bool
elem'' e [] = False
elem'' e (x:xs) = x==e || elem e xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' xs = foldr (++) [] xs

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (xs:xss) = xs ++ concat xss


powers :: Integral a => a -> [a]
powers n = n : map (* n) (powers n)

divisors_of :: Integral a => a -> [a]
divisors_of x = [ n | n <- [1..x-1], x `mod` n == 0 ]


getLastElem :: [a] -> a
getLastElem [x] = x
getLastElem (_:xs)  = getLastElem xs

getButLast :: [a] -> a
getButLast [x,y] = x
getButLast (_:x:xs) = getButLast (x:xs)

-- lambda x . x = Identity function
identit :: Int -> Int
identit x = x

applyN = (foldr (.) id.) . replicate

zs :: [(Integer, Integer)]
zs = [(x,y) | x <- naturals, y <- naturals]

ones :: [Integer]
ones = 1 : ones

numsFrom :: Integer -> Integer
numsFrom n = n + numsFrom (n+1)

dupeFirstInList s@(x:xs) = x:s

sign x | x > 0  = 1
       | x == 0 = 0
       | x < 1  = -1


--- case expression ---
ttake m ys = case (m, ys) of
                 (0,_)    -> []
                 (_,[])   -> []
                 (n,x:xs) -> x: take (n-1) xs


--- client-server model ----
reqs = client initial resps
resps = server reqs

client initial ~(rp:resps) = initial : client (next rp) resps
server (req:reqs)      = process req: server reqs

initial = 0
next resp = resp
process req = req + 1

--digits :: Num a => a -> Integer

-- priemgetallen
primes = 2 : gaps 3 (join [[p*p,p*p+2*p..] | p <- primes'])
   where
     primes' = 3 : gaps 5 (join [[p*p,p*p+2*p..] | p <- primes'])
     join  ((x:xs):t)        = x : union xs (join (pairs t))
     pairs ((x:xs):ys:t)     = (x : union xs ys) : pairs t
     gaps k xs@(x:t) | k==x  = gaps (k+2) t
                     | True  = k : gaps (k+2) xs

{- 'union' function is implemented in Data.list
union (x:xs) (y:ys) = case (compare x y) of
        LT -> x : union  xs  (y:ys)
        EQ -> x : union  xs     ys
        GT -> y : union (x:xs)  ys
-}


--isLychrel :: Int -> Bool
isLychrel x | hasPalindromes (take 50 $ iterate reverseadd x)  = False
            | otherwise                                        = True


--hasPalindromes :: [Int] -> Bool
hasPalindromes xs = (length $ filter (==True) $ map isPalindromic xs) > 0

--isPalindromic :: Int -> Bool
isPalindromic x = x == ((read . reverse . show) x :: Integer)

--reverseadd :: Int -> Int
reverseadd x =  x + (read . reverse . show $ x :: Integer)

naturals :: (Num a, Enum a) => [a]
naturals = [1..]

lltseqns :: [Integer]
lltseqns = 4 : [ x^2 - 2 | x <- lltseqns ]

--See:https://en.wikipedia.org/wiki/Lucas–Lehmer_primality_test
--isLLTPrimalityTest :: (Integer, Integer) -> Bool
isLLTPrimalityTest (p,x)
    | p == 1                                     = False
    | (map (`mod` x) lltseqns) !! (p - 2) == 0   = True
    | otherwise                                  = False


mersenneprimes :: [(Int, Integer)]
mersenneprimes = filter isLLTPrimalityTest $ zip naturals mnumbers

mnumbers :: [Integer]
mnumbers = map (subtract 1) $ powers 2

powersoftwo :: [Integer]
powersoftwo = [2^x | x <- [1,2..] ]
--
--todo https://en.wikipedia.org/wiki/Miller–Rabin_primality_test
--
deelDoorTweeLijst :: Float -> [Float]
deelDoorTweeLijst x = x : deelDoorTweeLijst (x/2.0)


data Set a = Set [a]

empty :: Set a
empty = Set []

insert :: Integer -> Set Integer -> Set Integer
insert x (Set xs)
    | not (x `elem` xs) = Set (x:xs)
    | otherwise         = Set xs

-- Record Syntax
--
-- this is an example that doesn't use record syntax
data Point = Point Int Int deriving Show

xval :: Point -> Int
xval (Point x _) = x

yval :: Point -> Int
yval (Point _ y) = y

-- The origin would be the following Point

the_origin = Point 0 0

-- But also consider the following Pointt using Record Syntax, which in some
-- cases is more readable:
data Pointt = Pointt {
                       xvall :: Double,
                       yvall :: Double
                     }
                     deriving (Show)

the_originn = Pointt {xvall = 2, yvall = 0 }

-- De functie <filter''> kan gedefinieerd worden in termen van <concat> en <map>:

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = concat . map box
    where box x | p x       = [x]
                | otherwise = []


-- "<iterate'> takes a function that maps a type 'a' to a type 'a', and a type 'a'
-- and maps it to a list of type 'a'

iterate' :: (a -> a) -> a -> [a]     --  ghci> take 10 $ iterate' (\x -> 2*x) 2
iterate' f x = x : iterate' f (f x)  --  [2,4,8,16,32,64,128,256,512,1024]

-- a version of <repeat> :
repeat' :: a -> [a]        -- ghci> take 10 $ repeat 2
repeat' x = x : repeat' x  -- [2,2,2,2,2,2,2,2,2,2]

-- a non-recursive definition of <repeat> with <iterate'>:
repeat'' :: a -> [a]
repeat'' x = iterate' (\y -> y) x

(\\) :: Eq a => [a] -> [a] -> [a]
xs \\ ys = filter (`notElem` ys) xs


--Definieer de functie <curry'> als tegenhanger van <uncurry'>
curry'                   :: ((a, b) -> c) -> a -> b -> c
curry' f x y             =  f (x, y)
uncurry'                 :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f p             =  f (fst p) (snd p)


--Wat is het type van map map
--
--Geef een definitie van until
--Schrijf de functie length als aanroep van foldr


-- verzameling van Natuurlijke getallen.
sN = [1..]
ten = take 10
five =  take 5


-- Gebruik de functies map en concat in plaats van lijstcomprehensie om de volgende
-- lijst te definieren: [(x,y) | x <- [1..5], y <- [1..x]]
opgave3_7 = concat (map f [1..5])
            where f x = map g [1..x]
                        where g y = (x,y)


pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x,y)
--addInt :: Int a => a -> a -> a
--addInt x y = x >>= (\n ->
--             y >>= (\m ->
--              n + m ))
--
add :: Maybe Int -> Maybe Int -> Maybe Int
add mx my =             -- Adds two values of type (Maybe Int), where each input value can be Nothing
  mx >>= (\x ->         -- Extracts value x if mx is not Nothing
  my >>= (\y ->       -- Extracts value y if my is not Nothing
  return (x + y)))  -- Wraps value (x+y), returning the sum as a value of type (Maybe Int)

-- [[[a -> b]]] -> [[[[a] -> [b]]]]
--
mapp :: (a -> b) -> [a] -> [b]
mapp f [] = []
mapp f (x:xs) = f x : mapp f xs

groterDanTien :: Integer -> String
groterDanTien x | x <= 10   = "<=10"
                | otherwise = ">10"
