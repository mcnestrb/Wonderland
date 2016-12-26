-- I want these language extensions for my syntactic sugaring tricks at the end

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

-- I want my own definition of lookup and I want to write my own function
-- named "print".
module Wonderland (
Statement(..), Expr(..), Val(..)
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
type EvalEnv = Map.Map Name Val
type Env = Map.Map Name [Val]

lookup k t = case Map.lookup k t of
                Just x -> return x
                Nothing -> fail ("Unknown variable "++k)

-- {-- Monadic style expression evaluator,
--  -- with error handling and Reader monad instance to carry dictionary
--  --}

type Eval a = ReaderT EvalEnv (ExceptT String Identity) a
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


type Run a = StateT Env IO a
runRun p = runStateT p Map.empty

set :: Name -> Val -> Run ()
set n v = do st <- get
             let maybeVals = Map.lookup n st
             case maybeVals of
               Nothing -> state $ (\table -> ((), Map.insert n [v] table))
               Just maybeVals -> state $ (\table -> ((), Map.insert n (v:maybeVals) table))
{-
exec :: Statement -> Run ()
exec (Assign s v) = do st <- get
                       Right val <- return $ runEval st (eval v)
                       set s val
-}
