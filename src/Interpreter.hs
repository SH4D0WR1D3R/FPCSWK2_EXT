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
interpret (p:ps) mem = current p mem >>= interpret ps

current :: Stmt -> Memory -> Either Err Memory -- evaluates the current statement - pattern matches for different statement types
current (AssignStmt var ex) mem = editMemory var ex mem
current (IfStmt x t [] f) mem =  eval x mem>>= \a -> if a /= 0 then interpret t mem else interpret f mem
current (IfStmt x t [(ex, e)] f) mem = eval x mem>>= \a -> if a /= 0 then interpret t mem else eval ex mem >>= \b -> if b /= 0 then interpret e mem else interpret f mem
current (RepeatStmt ex xs) mem = do
     val <- eval ex mem
     m <- interpret xs mem
     if val <= 1 then Right m
     else current (RepeatStmt (ValE $ val-1) xs) m


editMemory :: String -> Expr -> Memory -> Either Err Memory
editMemory s x mem = eval x mem >>= \val -> case lookup s mem of -- BINDING GETS RID OF EITHER
     Nothing -> Right $ (s, val):mem
     Just _ -> Right $ replaceMemory s val mem

replaceMemory :: String -> Int -> Memory -> Memory
replaceMemory _ _ [] = []
replaceMemory s i (m:ms)
    | fst m == s = (s, i):ms
    | otherwise = m:replaceMemory s i ms


safediv :: Int -> Int -> Either Err Int
safediv _ 0 = Left DivByZeroError
safediv x y = Right $ x `div` y

-- WANT LOOKUP TO WORK ON EITHER TYPE TO MAKE IT WORK FOR MEMORY
-- case statement to reduce length of lines
eval :: Expr -> Memory -> Either Err Int
eval (ValE x) _ = Right x
eval (VarE x) mem = case lookup x mem of
     Nothing -> Left $ UninitialisedMemory x
     Just y -> Right y
eval (BinOpE Add a b) mem = subEval (+) a b mem
eval (BinOpE Sub a b) mem = subEval (-) a b mem
eval (BinOpE Mul a b) mem = subEval (*) a b mem
eval (BinOpE Div a b) mem = do
     x <- eval a mem
     y <- eval b mem
     x `safediv` y
eval (BinOpE Pow a b) mem = do
     x <- eval a mem
     y <- eval b mem
     if y < 0 then Left NegativeExponentError else Right (x ^ y)
eval (BinOpE Equal a b) mem = subEvalBin (==) a b mem
eval (BinOpE Neq a b) mem = subEvalBin (/=) a b mem
eval (BinOpE LessThan  a b) mem = subEvalBin (<) a b mem
eval (BinOpE LessOrEqual a b) mem = subEvalBin (<=) a b mem
eval (BinOpE GreaterThan  a b) mem = subEvalBin (>) a b mem
eval (BinOpE GreaterOrEqual a b) mem = subEvalBin (>=) a b mem

subEval :: (Int -> Int -> Int) -> Expr -> Expr -> Memory -> Either Err Int
subEval f a b mem = do
     x <- eval a mem
     y <- eval b mem
     return (f x y)

subEvalBin :: (Int -> Int -> Bool) -> Expr -> Expr -> Memory -> Either Err Int
subEvalBin f a b mem = do
     x <- eval a mem
     y <- eval b mem
     return (if f x y then 1 else 0)


-- FUNCTION TO CONVERT AN EITHER TO JUST AN INT


--------------------------------------------------------------------------------
