import Control.Monad.Writer
import Control.Monad ((<=<), (>=>))

deeldoorTwee :: (Integral a, Show a) => a -> Maybe String
deeldoorTwee x = if even x
                   then Just (show x++" is deelbaar door 2." )
                   else Just (show x++" is niet deelbaar door 2!" )

half :: (Integral a) => a -> Maybe a
half x = if even x
           then Just (x `div` 2)
           else Nothing

halveer :: Int -> Maybe Int
halveer x = do
    if even x
        then Just (x `div` 2)
        else Nothing

halveer' :: Int -> Maybe Int
halveer' x | even x    = Just (x `div` 2)
           | otherwise = Nothing

plus2 :: Int -> Maybe Int
plus2 x  = Just (x+2)

plus2enhalveer :: Int -> Maybe Int
plus2enhalveer = plus2 >=> halveer

halveerEnplus2 :: Int -> Maybe Int
halveerEnplus2 = plus2 <=< halveer

draaiom :: String -> Maybe String
draaiom s = Just (reverse s)

main =
    getLine    >>= \x ->
    putStrLn x >>= \_ ->
    return ()

main1 =
    getLine             >>= \x ->
    putStrLn (x++"hoi") >>= \_ ->
    return ()

main2 =
    getLine    >>= \x ->
      putStrLn x >>= \_ ->
        return ()

main4 =
    getLine >>= \x -> putStrLn (x++"hoi") >>= \_ -> return ()


theodds = do
   x <- [1..10]
   if odd x
       then [x * 2]
       else []


theodds' = [x*2 | x <-[1..10], odd x]

f :: Float -> Float
f = \x -> x^2
