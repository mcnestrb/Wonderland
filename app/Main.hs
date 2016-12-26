module Main where

--import Wonderland
import Wonderland
import System.Environment

main :: IO ()
main = do
          f <- getLine
          x <- readFile f
          let newStats = getStats x
          mapM_ putStrLn (map (show) newStats)

getStats :: String -> [Statement]
getStats x = putStatementsInList (read x)
{-
doThePrintThing :: IO ()
doThePrintThing = putStrLn $ show (Seq (Assign "varX" (Const (I 5))) (Assign "varX" ((Add (Var "varX") (Const (I 2))))) )
-}
putStatementsInList :: Statement -> [Statement]
putStatementsInList (Seq stat stats) = stat : (putStatementsInList stats)
putStatementsInList stat = [stat]
