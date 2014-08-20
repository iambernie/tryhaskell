-- Introduction.

-- ***** Functions *****

-- This is how you can define a function named 'foo'. It takes 1 argument,
-- namely 'x'.
foo x = x^2

-- Try not to interpret the '=' sign as an assignment operator which stores
-- some value in some variable. But rather read this as: "The function named
-- foo with argument x is defined by the expression x^2".
--
-- The reason I mention this is because expressions have a more central role
-- than what you may be used to. Actually, in more specific language, namely
-- in lambda calculus words,  'foo' is the variable (or identifier or name)
-- for the following lambda abstraction:
--
--     lambda x . x^2  (Not Haskell code, but lambda-calculus notation)
--
-- Some of Haskell's syntax mimics this language. To illustrate this, let
-- me rewrite foo as a lambda abstraction:
fooo = \x -> x^2
-- If you try these out, you'll see that foo and fooo behave exactly the same.
--
-- Functions can be composed with the dot operator.
foo_fooo = (foo . fooo)
-- So calling foo_fooo 2 will return 16


-- This is how you can define a function with 2 arguments:
bar x y = x^2 + y
-- And as a lambda abstraction it would look like:
barr = \x y -> x^2 + y

-- The format is the same for n number of arguments. I should also mention
-- that function names must begin with a lower case.

-- Functions can also be defined recursively, take for example the functions
-- factorial and fibonacci below.

factorial 0 = 1
factorial n = n * factorial (n-1)

-- or as:
ffactorial n |  n == 0      = 1
             |  otherwise   = n * factorial (n-1)

-- or even:
fffactorial n
    |  n == 0      = 1
    |  otherwise   = n * factorial (n-1)

-- or:
ffffactorial n = if n == 0 then 0 else n * factorial (n-1)

-- or:
fffffactorial n =
    if n == 0 then 0
    else n * factorial (n-1)

-- or:
ffffffactorial n =
    if n == 0
    then 0
    else n * factorial (n-1)

-- And the fibonacci numbers could be defined like this:
fibonacci n  | n == 0     = 1
             | n == 1     = 1
             | otherwise  = fibonacci (n-2) + fibonacci (n-1)

-- And there are many other ways to define them...
fibs = 1:1:[ a+b | (a,b) <- zip fibs (tail fibs)]

fib@(1:tfib) = 1:1:[ a+b | (a,b) <- zip fib tfib]
--
-- The syntax that uses the pipeline characters '|' (called guards in Haskell)
-- looks a lot like the way functions are written in calculus. For example, the
-- function:
--            /  x^2         for x  <  -2
--            |
--     h(x) = <  x + 1       for  -2 <=  x  <= 2
--            |
--            \  (x+1)^2     for x > 2
--
-- Could be written as:

h x  |  x < -2                   =   x^2
     |  -2 <= x  && x <= 2       =   x + 1
     |  otherwise                =   (x + 1)^2

-- You can also define functions within functions. Take for example
-- the function isPositive which could also be written as iisPositive.

isPositive x  |  x < 0    = False
              |  x == 0   = False
              |  x > 0    = True


iisPositive x  |  sign x == (-1)  = False
               |  sign x == 0     = False
               |  sign x == 1     = True
               where sign x  | x < 0    = (-1)
                             | x == 0   = 0
                             | x > 0    = 1

-- *****  Currying  *****
-- f a b c d
-- ((((f a) b) c) d)
--
-- *****  Constructors *****
--
data Booll = Trrue
           | Ffalse
           deriving Show


data Color = Red
           | Green
           | Blue
           deriving Show

data Colour = Redd Int
            | Greenn Int
            | Bluee Int
            deriving Show


--data Train a = EmptyWagon
--             | Attacher a
--
data Cargo = TypeA
           | TypeB
           | TypeC
           deriving Show

data Train wagon = ZeroTrain
                 | Attach wagon (Train wagon)
                 deriving Show

-- polymorphism

xs = [1..10]
--data Lst = EmptyLst | Appender a

-- *****  Type signature *****

-- *****  Lists  ****
--
--
--
--
--
--
--
--
--
--
--
--
