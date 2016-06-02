module Main where

import Data.EasyJSON
import Data.Maybe
import System.Environment
import Data.Aeson.Encode.Pretty

main :: IO ()
main = do
    args <- getArgs
    val <- readFileToJSON (head args)
    if isJust val 
    then putStrLn "No errors"
    else return ()

