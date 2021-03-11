--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
interpret (p:ps) mem = current p mem >>= interpret ps

-- | Given a statement and the memory contents, the function outputs the new
-- updated contents of the memory.
-- The function pattern matches against the different statement inputs and 
-- evaluates each statement accordingly.
current :: Stmt -> Memory -> Either Err Memory
current (AssignStmt var ex) mem = editMemory var ex mem
current (IfStmt x t [] f) mem =  eval x mem>>= \a -> if a /= 0 then interpret t mem else interpret f mem
current (IfStmt x t [(ex, e)] f) mem = eval x mem>>= \a -> if a /= 0 then interpret t mem else eval ex mem >>= \b -> if b /= 0 then interpret e mem else interpret f mem
{-current (IfStmt x t [(ex, e)] f) mem = do
     a <- eval x mem
     b <- eval ex mem
     if a /= 0 then interpret t mem else b
     if b /= 0 then interpret e mem else interpret f mem-}

current (RepeatStmt ex xs) mem = do
     val <- eval ex mem
     m <- interpret xs mem
     if val <= 1 then Right m
     else current (RepeatStmt (ValE $ val-1) xs) m

-- | This helper function is used when variable assignment takes place, and updates the
-- memory accordingly. If the variable already exists in memory, that value is updated,
-- else, it is added to the beginning of the memory store - more efficient to use cons
-- rather than ++, which runs through the whole list to add to the end.
editMemory :: String -> Expr -> Memory -> Either Err Memory
editMemory s x mem = eval x mem >>= \val -> case lookup s mem of -- BINDING GETS RID OF EITHER
     Nothing -> Right $ (s, val):mem
     Just _ -> Right $ replaceMemory s val mem

-- | This is called when there is a value in memory with that variable name already.
-- It updates the pair value stored in memory, and keeps it in the same position as
-- it was.
replaceMemory :: String -> Int -> Memory -> Memory
replaceMemory _ _ [] = []
replaceMemory s i (m:ms)
    | fst m == s = (s, i):ms
    | otherwise = m:replaceMemory s i ms

-- | This is used to evaluate expressions passed into it accordingly. It pattern matches
-- for each Expr type, and outputs something accordingly. ValE is meant to just return the 
-- value it holds, VarE performs a memory lookup for the value associated with the variable,
-- and BinOpE performs the relevant operation on the two expressions passed in.
eval :: Expr -> Memory -> Either Err Int
eval (ValE x) _ = Right x
eval (VarE x) mem = case lookup x mem of
     Nothing -> Left $ UninitialisedMemory x
     Just y -> Right y
eval (BinOpE f a b) mem = case f of
     Add -> subEval (+) a b mem
     Sub -> subEval (-) a b mem
     Mul -> subEval (*) a b mem
     Div -> do
          x <- eval a mem
          y <- eval b mem
          x `safediv` y
     Pow -> do
          x <- eval a mem
          y <- eval b mem
          if y < 0 then Left NegativeExponentError else Right (x ^ y)
     Equal -> subEvalBin (==) a b mem
     Neq -> subEvalBin (/=) a b mem
     LessThan -> subEvalBin (<) a b mem
     LessOrEqual -> subEvalBin (<=) a b mem
     GreaterThan -> subEvalBin (>) a b mem
     GreaterOrEqual -> subEvalBin (>=) a b mem

-- | This method is here to help with readability, as it avoids repeating code unnecessarily in the above function.
-- | This function is for arithmetic operators.
subEval :: (Int -> Int -> Int) -> Expr -> Expr -> Memory -> Either Err Int
subEval f a b mem = do
     x <- eval a mem
     y <- eval b mem
     return (f x y)

-- | This is the same purpose as the function above, however, it is used on binary operators,
-- rather than arithmetic operators.
subEvalBin :: (Int -> Int -> Bool) -> Expr -> Expr -> Memory -> Either Err Int
subEvalBin f a b mem = do
     x <- eval a mem
     y <- eval b mem
     return (if f x y then 1 else 0)

-- | This function is here to ensure that we don't accidentally divide by zero, and 
-- if zero is passed in to be divided by, the relevant error is returned.
safediv :: Int -> Int -> Either Err Int
safediv _ 0 = Left DivByZeroError
safediv x y = Right $ x `div` y

--------------------------------------------------------------------------------
