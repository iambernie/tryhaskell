
recsum :: Integral a => a -> a
recsum 0 = 0
recsum x = x + recsum (x-1)


recsuml :: Integral a => a -> a
recsuml n = foldl (+) 0 [1..n]

recsumr :: Integral a => a -> a
recsumr n = foldr (+) 0 [1..n]

foldl' f z []     = z
foldl' f z (x:xs) = let z' = z `f` x
                    in seq z' $ foldl' f z' xs

foldl'' f z []     = z
foldl'' f z (x:xs) = seq z' $ foldl' f z' xs
                   where z' = z `f` x

foldl''' f z []     = z
foldl''' f z (x:xs) = seq (z `f` x) $ foldl' f (z `f` x) xs

foldl'''' f z []     = z
foldl'''' f z (x:xs) = seq (f z x) $ foldl' f (f z x) xs

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x `seq` (x : iterate' f (f x))


iter n = foldl' (\acc f -> f acc) 0.3 (replicate n f)
iter' n = foldl' (\acc _ -> f acc) 0.3 [1..n]
iter'' n = foldl' (flip ($)) 0.3 . take n $ repeat f
