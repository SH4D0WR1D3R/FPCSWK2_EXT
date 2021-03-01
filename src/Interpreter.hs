--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

module Interpreter where

--------------------------------------------------------------------------------

import Language

--------------------------------------------------------------------------------

-- | In our memory, named locations map to values.
type Memory = [(String, Int)]

-- | Enumerates reasons for errors.
data Err
    = DivByZeroError                    -- ^ Division by zero was attempted.
    | NegativeExponentError             -- ^ Raising a number to a negative
                                        -- exponent was attempted.
    | UninitialisedMemory String        -- ^ Tried to read from a variable
                                        -- that does not exist.
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Given a program and the initial memory contents, determines
-- what evaluating the program does to the memory.
interpret :: Program -> Memory -> Either Err Memory
interpret [] mem = Right mem
-- might be a case with no memory?
interpret (p:ps) mem = undefined

current :: Stmt -> Memory -> Either Err Memory -- evaluates the current statement - pattern matches for different statement types
current (AssignStmt var ex) mem = undefined
--current (IfStmt Expr [Stmt] [(Expr, [Stmt])] [Stmt]) mem = undefined 
--current (RepeatStmt Expr [Stmt]) mem = undefined

{-myLookup :: Expr -> Memory -> Either Err Int
myLookup (VarE x) mem = case lookup x mem of 
     Nothing -> Left UninitialisedMemory x
     Just y -> Right y-}

eval :: Expr -> Memory -> Either Err Memory
eval (ValE x) _ = Right x
eval (VarE x) mem = (VarE x) mem = case lookup x mem of 
     Nothing -> Left UninitialisedMemory x
     Just y -> Right y

-- FUNCTION TO CONVERT AN EITHER TO JUST AN INT

{-eval :: Expr -> Memory -> Maybe Int
eval (ValE x) _ = Just x
--evalBin (VarE x) (m:ms) = if fst m == then snd m else evalBin (VarE x) ms
eval (VarE x) mem = lookup x mem
eval (BinOpE o x y) mem = Just (operation o (eval x mem) (eval y mem))

operation :: Op -> Maybe Int -> Maybe Int -> Int
operation Add (Just x) (Just y) = x + y
operation Sub (Just x) (Just y) = x - y
operation Mul (Just x) (Just y) = x * y
operation Div (Just x) (Just y) = x / y
operation Pow (Just x) (Just y) = x ^ y
operation Equal (Just x) (Just y) = if x == y then 1 else 0
operation Neq (Just x) (Just y) = if x /= y then 1 else 0
operation LessThan (Just x) (Just y) = if x < y then 1 else 0
operation LessOrEqual (Just x) (Just y) = if x <= y then 1 else 0
operation GreaterThan (Just x) (Just y) = if x > y then 1 else 0
operation GreaterOrEqual (Just x) (Just y)  = if x >= y then 1 else 0
-}

--------------------------------------------------------------------------------
