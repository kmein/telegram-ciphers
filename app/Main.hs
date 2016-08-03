module Main where

import Lib (run)
import System.Directory (doesFileExist, makeAbsolute)
import System.Environment (getArgs)

main :: IO ()
main =
    do args <- getArgs
       case args of
         [filePath] ->
             do absPath <- makeAbsolute filePath
                exists <- doesFileExist absPath
                if exists
                then run absPath
                else putStrLn "The file you specified does not exist."
         _ -> putStrLn "Invalid arguments. Supply one file path, please."
