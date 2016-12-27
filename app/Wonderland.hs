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

exec :: Statement -> Run ()
exec (Assign s v) = do st <- get
                       Right val <- return $ runEval st (eval v)
                       liftIO $ System.print $ "Next line to execute: " ++ show (Assign s v)
                       letter <- liftIO $ getLine
                       case letter of
                         "n" -> set s val
                         _   -> do liftIO $ System.print $ "Not a valid input - press n to step forward"
                                   exec (Assign s v)

exec (Print e) = do st <- get
                    Right val <- return $ runEval st (eval e)
                    liftIO $ System.print $ "Next line to execute: " ++ show (Print e)
                    letter <- liftIO $ getLine
                    case letter of
                      "n" -> do liftIO $ System.print val
                                return ()
                      _   -> do liftIO $ System.print $ "Not a valid input - press n to step forward"
                                exec (Print e)
{-
exec (If cond s0 s1) = do st <- get
                          Right (B val) <- return $ runEval st (eval cond)
													liftIO $ System.print $ show (If cond s0 s1)
													letter <- liftIO $ getLine
													case letter of
														"n" -> do liftIO $ if val then do exec s0 else do exec s1
														_   -> do liftIO $ System.print $ "Not a valid input - press n to step forward"
			                                exec (If cond s0 s1)
-}
step :: [Statement] -> Run ()
step [] = return ()
step (stat:stats) = do exec stat
                       step stats

run :: [Statement] -> IO ()
run stats = do result <- runExceptT $ (runStateT $ step stats) Map.empty
               case result of
                 Right ((), env) -> return ()
                 Left exn -> System.print ("Uncaught exception " ++ exn)
