-- I want these language extensions for my syntactic sugaring tricks at the end

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

-- I want my own definition of lookup and I want to write my own function
-- named "print".
module Wonderland (
run, Statement(..)
) where
import Prelude hiding (lookup, print)

import qualified Data.Map as Map
import Data.Maybe

-- I want to get at the standard "print" function using the name System.print

import qualified System.IO as System

-- I plan to use these monads to construct the parts of my interpreter

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

-- {-------------------------------------------------------------------}
-- {- The pure expression language                                    -}
-- {-------------------------------------------------------------------}

data Val = I Int | B Bool
            deriving (Eq, Show, Read)

data Expr = Const Val
     | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
     | And Expr Expr | Or Expr Expr | Not Expr
     | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
     | Var String
     deriving (Eq, Show, Read)

type Name = String
type Env = Map.Map Name [Val]

lookup k t = case Map.lookup k t of
                Just x -> return $ head x
                Nothing -> fail ("Unknown variable "++k)

lookupHistory k t = case Map.lookup k t of
                      Just x -> return x
                      Nothing -> fail ("Unknown variable "++k)

-- {-- Monadic style expression evaluator,
--  -- with error handling and Reader monad instance to carry dictionary
--  --}

type Eval a = ReaderT Env (ExceptT String Identity) a
runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )

-- This evaluator could be a little neater

-- Integer typed expressions

evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                          (I i0, I i1) -> return $ I (i0 `op` i1)
                          _            -> fail "type error in arithmetic expression"

-- Boolean typed expressions

evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                          (B i0, B i1) -> return $ B (i0 `op` i1)
                          _            -> fail "type error in boolean expression"

-- Operations over integers which produce booleans

evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                           (I i0, I i1) -> return $ B (i0 `op` i1)
                           _            -> fail "type error in arithmetic expression"

-- Evaluate an expression

eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = do evali (+) e0 e1
eval (Sub e0 e1) = do evali (-) e0 e1
eval (Mul e0 e1) = do evali (*) e0 e1
eval (Div e0 e1) = do evali div e0 e1

eval (And e0 e1) = do evalb (&&) e0 e1
eval (Or e0 e1) = do evalb (||) e0 e1

eval (Not e0  ) = do evalb (const not) e0 (Const (B True))
                        where not2 a _ = not a -- hack, hack

eval (Eq e0 e1) = do evalib (==) e0 e1
eval (Gt e0 e1) = do evalib (>) e0 e1
eval (Lt e0 e1) = do evalib (<) e0 e1

eval (Var s) = do env <- ask
                  lookup s env


-- {-------------------------------------------------------------------}
-- {- The statement language                                          -}


data Statement = Assign String Expr
                 | If Expr Statement Statement
                 | While Expr Statement
                 | Print Expr
                 | Seq Statement Statement
                 | Try Statement Statement
                 | Pass
                 deriving (Eq, Show, Read)


type Run a = StateT Env (ExceptT String IO) a
runRun p = runExceptT (runStateT p Map.empty)

set :: Name -> Val -> Run ()
set n v = do st <- get
             case Map.lookup n st of
               Nothing -> state $ (\table -> ((), Map.insert n [v] table))
               Just maybeVals -> state $ (\table -> ((), Map.insert n (v:maybeVals) table))

unset :: Name -> Run ()
unset n = do st <- get
             case Map.lookup n st of
               Nothing -> return ()
               Just list -> state $ (\table -> ((), Map.insert n (tail list) table))

exec :: Statement -> [Statement] -> Run ()
exec (Seq s0 s1) doneStats = do exec s0 doneStats >> exec s1 doneStats

exec (Assign s v) _ = do st <- get
                         Right val <- return $ runEval st (eval v)
                         set s val

exec (Print e) _ = do st <- get
                      Right val <- return $ runEval st (eval e)
                      liftIO $ System.print val
                      return ()

exec (If cond s0 s1) doneStats = do st <- get
                                    Right (B val) <- return $ runEval st (eval cond)
                                    if val then do exec s0 doneStats else do exec s1 doneStats

exec (While cond s) doneStats = do st <- get
                                   Right (B val) <- return $ runEval st (eval cond)
                                   if val then do exec s doneStats >> exec (While cond s) doneStats else return ()

exec (Try s0 s1) doneStats = do catchError (exec s0 doneStats) (\e -> exec s1 doneStats)

stepBack :: [Statement] -> Run ()
stepBack [] = liftIO $ System.print "Nothing has been executed, cannot step back"

stepBack ((Assign s _):_) = unset s

stepBack ((Print _):_) = return ()

step :: [Statement] -> [Statement] -> Run ()
step [] _ = return ()
step (stat:stats) doneStats = do st <- get
                                 liftIO $ liftIO $ System.print $ "Next line to execute: " ++ show stat
                                 input <- liftIO $ getLine
                                 case input of
                                   "n" -> do exec stat doneStats
                                             step stats (stat:doneStats)
                                   "i" -> do liftIO $ System.print "Variables available to inspect:"
                                             let currKeys = (Map.keys st)
                                             liftIO $ mapM_ putStrLn (map show currKeys)
                                             inspect <- liftIO $ getLine
                                             case lookupHistory inspect st of
                                               Just value -> do liftIO $ putStrLn $ show value
                                                                step (stat:stats) doneStats
                                               Nothing -> do liftIO $ putStrLn "Not a valid variable"
                                                             step (stat:stats) doneStats
                                   "u" -> do stepBack doneStats
                                             case length doneStats of
                                               0 -> step (stat:stats) doneStats
                                               _ -> step ((head doneStats):stat:stats) (tail doneStats)
                                   _   -> do liftIO $ System.print $ "Not a valid input - press n to step forward, i to inspect or u to step backward"
                                             step (stat:stats) doneStats

run :: [Statement] -> IO ()
run stats = do result <- runExceptT $ (runStateT $ step stats []) Map.empty
               case result of
                 Right ((), env) -> return ()
                 Left exn -> System.print ("Uncaught exception " ++ exn)
