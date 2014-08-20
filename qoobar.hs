module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)


main :: IO ()
main = do
    args <- getArgs
--    putStrLn ("Args: " ++ (args !! 0)++ " Args1: " ++ (args !! 1))
--    putStrLn ( show ((read (args !! 0)) + (read (args !! 1))) )
--    gottenline <- getLine
--    putStrLn gottenline
    putStrLn (readExpr (head args))


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No Match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space


















