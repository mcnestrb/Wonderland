module Main where

--import Wonderland
import Wonderland
import System.Environment

-- read the file and run it
main :: IO ()
main = do f <- getLine
          x <- readFile f
          let newStats = getStats x
          run newStats

-- read the input string as a Statement
getStats :: String -> [Statement]
getStats x = putStatementsInList (read x)

-- chop up any statements that are Seqs into a list of Statements
putStatementsInList :: Statement -> [Statement]
putStatementsInList (Seq stat stats) = stat : (putStatementsInList stats)
putStatementsInList stat = [stat]
