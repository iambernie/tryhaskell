import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

--------------  A writer example  ---------------
type MyWriter = Writer [Int] String

example :: MyWriter
example  = do
  tell [1..5]
  tell [5..10]
  return "foo"

output :: (String, [Int])
output = runWriter example


-------------  A reader example  ----------------
computation :: Reader MyContext (Maybe String)
computation = do
  n <- asks bar
  x <- asks foo
  if n > 0
    then return (Just x)
    else return Nothing

--------- Not a reader ------------
computation' :: MyContext -> Maybe String
computation' context = let x = foo context
                           n = bar context in
                       if n > 0
                       then (Just x)
                       else Nothing

-------  A reader example ---------------


greeter :: Reader Int String
greeter = do
    name <- ask
    return ("hello!" )

hello :: Reader String String
hello = do
    name <- ask
    return ("hello, " ++ name ++ "!")

bye :: Reader String String
bye = do
    name <- ask
    return ("bye, " ++ name ++ "!")

convo :: Reader String String
convo = do
    c1 <- hello
    c2 <- bye
    return $ c1 ++ c2

all = print . runReader convo $ "adit"



ex1 :: Maybe String
ex1 = runReader computation $ MyContext "hello" 1

ex2 :: Maybe String
ex2 = runReader computation $ MyContext "haskell" 0




{- State Example -}
test :: State Int Int
test = do
  put 3
  modify (+1)
  get

main :: IO ()
main = print $ execState test 0

{- ReaderT -}

type Env = [(String, Int)]
type Eval a = ReaderT Env Maybe a

data Expr
  = Val Int
  | Add Expr Expr
  | Var String
  deriving (Show)

eval :: Expr -> Eval Int
eval ex = case ex of

  Val n -> return n

  Add x y -> do
    a <- eval x
    b <- eval y
    return (a+b)

  Var x -> do
    env <- ask
    val <- lift (lookup x env)
    return val

env :: Env
env = [("x", 2), ("y", 5)]

ex1a :: Eval Int
ex1a = eval (Add (Val 2) (Add (Val 1) (Var "x")))

example1, example2 :: Maybe Int
example1 = runReaderT ex1a env
example2 = runReaderT ex1a []


-------- Data types used in examples ------------------

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

data MyContext = MyContext
  { foo :: String
  , bar :: Int
  } deriving (Show)

data Person = Person { first :: String
                     , last :: String
                     , age :: Int
                     , birthdate :: String
                     } deriving (Show)

data Circle = Cirle { radius :: Double } deriving (Show)


