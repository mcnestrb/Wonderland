module Main where

import Wonderland
import System.Environment

main :: IO ()
main = do
         args <- getArgs
         content <- readFile (args !! 0)
         let linesOfFile = lines content
         printOutAllTheStrings linesOfFile

printOutAllTheStrings :: [String] -> IO ()
printOutAllTheStrings [x] = putStrLn x
printOutAllTheStrings (x:xs) = do putStrLn x
                                  printOutAllTheStrings xs
