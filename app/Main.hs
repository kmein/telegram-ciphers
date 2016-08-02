module Main where

import Lib (mainLoop, initialOptions)

main :: IO ()
main = mainLoop initialOptions Nothing
