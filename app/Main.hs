module Main where

--import Wonderland
import Wonderland
import System.Environment

main :: IO ()
main = do f <- getLine
          x <- readFile f
          let newStats = getStats x
          run newStats

getStats :: String -> [Statement]
getStats x = putStatementsInList (read x)

putStatementsInList :: Statement -> [Statement]
putStatementsInList (Seq stat stats) = stat : (putStatementsInList stats)
putStatementsInList stat = [stat]
