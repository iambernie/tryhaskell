
class  Functor f  where
    fmap  :: (a -> b) -> f a -> f b

{- All instances of Functor should obey:
    fmap id      = id
    fmap (p . q) = (fmap p) . (fmap q)   -}


class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    return :: a -> m a
    fail   :: String -> m a

{-  All instances of Monad should obey the Monad Laws:

(return x) >>= f        = f x
         m >>= return   = m
 (m >>= f) >>= g        = m >>= ( \x -> (f x >>= g) )

or in do-notation:

 do { f x }                   = do { v <- return x; f v }
 do { m }                     = do { v <- m; return v }
 do { x <- m; y <- f x; g y } = do { y <- do { x <- m; f x }; g y }
-}


class (Functor f) => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

class (Functor f, Foldable f) => Traversable f where
  traverse :: Applicative g => f (g a) -> g (f a)

class Foldable f where
  foldMap :: Monoid m => (a -> m) -> f a -> m

class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a

{- Monoids should satisfy the following laws:
   mappend mempty x        = x
   mappend x mempty        = x
   mappend x (mappend y z) = mappend (mappend x y) z
   mconcat                 = foldr mappend mempty -}


class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

{- instances of MonadPlus are expected to satisfy:

    mzero >>= f  =  mzero
    v >> mzero   =  mzero
                                                   -}

{-                    Maybe                   -}

instance  Functor Maybe  where
    fmap f Nothing    =  Nothing
    fmap f (Just x)   =  Just (f x)

instance Monad Maybe where
    Nothing >>= f  = Nothing
    Just x  >>= f  = f x
    return x       = Just x
    fail _         = Nothing

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something


{-                     []                    -}

instance  Functor []  where
    fmap = map

instance  Monad []  where
    m >>= k    = foldr ((++) . k) [] m
    m >> k     = foldr ((++) . (\ _ -> k)) [] m
    return x   = [x]
    fail _     = []

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]



{-                     (->) r                    -}
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))

instance Functor ((->) r) where
    fmap = (.)


instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)

instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)

class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

class Monoid a where
    mempty  :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a

instance Monoid [a] where
    mempty = []
    mappend = (++)
