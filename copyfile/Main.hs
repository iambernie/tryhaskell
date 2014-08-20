
import System.IO

{-
    data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
    openFile :: FilePath -> IOMode -> IO Handle
    hGetContents :: Handle -> IO String
    hPutStr :: Handle -> String -> IO ()
-}

main :: IO ()
main = openFile "loremipsum" ReadMode  >>= \fromHandle ->
       openFile "ipsumlorem" WriteMode >>= \toHandle ->
       hGetContents fromHandle         >>= \contents ->
       hPutStr toHandle contents >>
       hClose toHandle

       


