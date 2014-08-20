data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

type State = Int

data ST a = Sconstruct (State -> (a, State))

instance Monad ST where
  return x = Sconstruct (\s -> (x,s))
  st (>>=) f = Sconstruct (\s -> let (x,s') = apply st s in apply (f x) s')

fresh :: ST Int
fresh = Sconstruct (\n -> (n, n+1))

mlabel' :: Tree a -> ST (Tree (a,Int))
mlabel' (Leaf x)          = do n <- fresh
                               return (Leaf (x,n))
mlabel' (Node left right) = do left' <- mlabel' left
                               right' <- mlabel' right
                               return (Node left' right')

apply :: ST a -> State -> (a,State)
apply (Sconstruct f) x = f x
