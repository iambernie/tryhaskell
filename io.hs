{-
getChar :: IO Char
putChar :: Char -> IO ()
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>) :: Monad m => m a -> m b -> m b
return :: Monad m => a -> m a
sequence_ :: Monad m => [m a] -> m ()
sequence :: Monad m => [m a] -> m [a]
-}

echo :: IO ()
echo = getChar >>= putChar
-- Prelude> echo   (then after hitting 'z' once)
-- zzPrelude>

echoTwice :: IO ()
echoTwice = echo >> echo

echoDup :: IO ()
echoDup = getChar >>= \c -> putChar c >> putChar c

echoDup' :: IO ()
echoDup' = getChar >>= \c ->
           putChar c >>
           putChar c

echoDup'' :: IO ()
echoDup'' = getChar >>= \c ->
            putChar c >>
            putChar c

getTwoChars :: IO (Char, Char)
getTwoChars = getChar >>= \c1 ->
              getChar >>= \c2 ->
              return (c1, c2)
-- Note that:
-- return :: a -> m a
-- So in this case:  (Char, Char) -> IO (Char,Char)

putTwoChars :: (Char, Char) -> IO ()
putTwoChars (c1, c2) = putChar c1 >> putChar c2

putTwoChars' :: (Char, Char) -> IO ()
putTwoChars' (c1, c2) = do {putChar c1 ; putChar c2}


putTwoChars'' :: (Char, Char) -> IO ()
putTwoChars'' (c1, c2) = do putChar c1
                            putChar c2

echoTwoChars :: IO ()
echoTwoChars = getTwoChars >>= putTwoChars

echoFourChars :: IO ()
echoFourChars = getChar >>= \c1 ->
                getChar >>= \c2 ->
                getChar >>= \c3 ->
                getChar >>= \c4 ->
                putChar c1 >>
                putChar c2 >>
                putChar c3 >>
                putChar c4

echoFourChars' :: IO ()
echoFourChars' = getChar >>= \c1 ->
                 getChar >>= \c2 ->
                 getChar >>= \c3 ->
                 getChar >>= \c4 ->
                 do
                     putChar c1
                     putChar c2
                     putChar c3
                     putChar c4

ready :: IO Bool
ready = getChar >>= \c ->
        return (c == 'y')

getLine' :: IO String
getLine' = do
             c <- getChar
             if c == '\n'
                 then return ""
             else 
                 do 
                   l <- getLine
                   return (c:l)




