import Control.Monad.Loops

type World = Char

stopCondition :: World -> Bool
stopCondition w = if w == 'q' then True else False

displayWorld :: World -> IO ()
displayWorld w = putStrLn ("You pressed: "++ [w])

gameLoop :: World -> IO World
gameLoop w = getChar


displayLoop :: World -> IO World
displayLoop w = displayWorld w >> (gameLoop w)

loop :: IO ()
loop = do initWorld <- getChar
          iterateUntilM stopCondition displayLoop initWorld
          return ()

main :: IO ()
main = loop

