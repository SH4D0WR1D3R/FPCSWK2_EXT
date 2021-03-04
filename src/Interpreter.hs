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
--current [] mem = Right mem
current (AssignStmt var ex) mem = editMemory var ex mem-- UNSURE
--current (IfStmt x (t:ts) [ex, (e:es)] (f:fs)) mem = eval a >>= \b -> if b /= 0 then eval t else (eval ex >>= \c -> if c /= 0 then eval e else eval f)
--current (IfStmt Expr [Stmt] [(Expr, [Stmt])] [Stmt]) mem = undefined 
--current (RepeatStmt Expr [Stmt]) mem = undefined

editMemory :: String -> Expr -> Memory -> Either Err Memory
editMemory s x mem = eval x mem >>= \val -> case lookup s mem of -- BINDING GETS RID OF EITHER
     Nothing -> Right $ (s, val):mem
     Just _ -> Right $ replaceMemory s val mem

replaceMemory :: String -> Int -> Memory -> Memory
replaceMemory _ _ [] = []
replaceMemory s i (m:ms)
    | fst m == s = (s, i):ms
    | otherwise = m:(replaceMemory s i ms)

{-myLookup :: Expr -> Memory -> Either Err Int
myLookup (VarE x) mem = case lookup x mem of 
     Nothing -> Left UninitialisedMemory x
     Just y -> Right y-}

safediv :: Int -> Int -> Either Err Int
safediv _ 0 = Left DivByZeroError
safediv x y = Right $ x `div` y

-- WANT LOOKUP TO WORK ON EITHER TYPE TO MAKE IT WORK FOR MEMORY

eval :: Expr -> Memory -> Either Err Int
eval (ValE x) _ = Right x
eval (VarE x) mem = case lookup x mem of 
     Nothing -> Left $ UninitialisedMemory x
     Just y -> Right y
eval (BinOpE Add a b) mem = do
     x <- eval a mem
     y <- eval b mem
     return $ x + y -- SPLIT UP BIN OP PATTERN MATCHING TO SEPARATE FUNCTION
eval (BinOpE Sub a b) mem = do
     x <- eval a mem
     y <- eval b mem
     return $ x - y
eval (BinOpE Mul a b) mem = do
     x <- eval a mem
     y <- eval b mem
     return $ x * y
eval (BinOpE Div a b) mem = do
     x <- eval a mem
     y <- eval b mem
     x `safediv` y
eval (BinOpE Pow a b) mem = do
     x <- eval a mem
     y <- eval b mem
     return $ x ^ y
eval (BinOpE Equal a b) mem = do
     x <- eval a mem
     y <- eval b mem
     return $ (if x == y then 1 else 0)
eval (BinOpE Neq a b) mem = do
     x <- eval a mem
     y <- eval b mem
     return $ (if x /= y then 1 else 0)
eval (BinOpE LessThan  a b) mem = do
     x <- eval a mem
     y <- eval b mem
     return $ (if x < y then 1 else 0)
eval (BinOpE LessOrEqual a b) mem = do
     x <- eval a mem
     y <- eval b mem
     return $ (if x <= y then 1 else 0)
eval (BinOpE GreaterThan  a b) mem = do
     x <- eval a mem
     y <- eval b mem
     return $ (if x > y then 1 else 0)
eval (BinOpE GreaterOrEqual a b) mem = do
     x <- eval a mem
     y <- eval b mem
     return $ (if x >= y then 1 else 0)


-- FUNCTION TO CONVERT AN EITHER TO JUST AN INT

--evalBin :: Op -> Expr -> Expr 
--evalBin 








{-eval :: Expr -> Memory -> Maybe Int
eval (ValE x) _ = Just x
--evalBin (VarE x) (m:ms) = if fst m == then snd m else evalBin (VarE x) ms
eval (VarE x) mem = lookup x mem
eval (BinOpE o a b) mem = Just (operation o (eval a mem) (eval b mem))

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
