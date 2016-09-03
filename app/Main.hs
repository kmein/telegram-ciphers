module Main where

import Lib (run)
import System.Directory (doesFileExist, makeAbsolute)
import System.Environment (getArgs)
import qualified Data.Text as T (strip)
import qualified Data.Text.IO as T (readFile)

main :: IO ()
main =
    do args <- getArgs
       case args of
         [filePath] ->
             do absPath <- makeAbsolute filePath
                exists <- doesFileExist absPath
                if exists
                then run . T.strip =<< T.readFile absPath
                else putStrLn "The file you specified does not exist.\n\
                    \(Note: home directory (tilde) expansion is not supported yet.)"
         _ -> putStrLn "Invalid arguments. Supply one file path, please."
