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

-- | Given a program and the initial memory contents, determines what evaluating the program does to the memory.
-- The interpret function takes a program, and a memory store, and executes each statement in the program, using the memory accordingly.
-- There is a pattern match in place to check for an empty program - i.e. an empty list - and returns the memory, since there wouldn't be an error when nothing is needing to be executed.
-- This empty list also acts as an edge case for the recursive interpret.
-- The line that takes a program with statements loops through the list, executing each statement accordingly. This applies the current function to the head of the memory list, and 
-- binds the result (the output memory) to the application of interpret on the tail of the list.
interpret :: Program -> Memory -> Either Err Memory
interpret [] mem = Right mem
interpret (p:ps) mem = current p mem >>= interpret ps

-- | Given a statement and the memory contents, the function outputs the new updated contents of the memory.
-- The function pattern matches against the different statement inputs and evaluates each statement accordingly.
-- If the statement passed in is an AssignStmt, the memory is updated using the arguments passed in, and applying editMemory to these arguments.
-- If the statement passed in is an IfStmt, the definition is
-- IfStmt :: Expr -> [Stmt] -> [(Expr, [Stmt])] -> [Stmt]
-- There are 2 cases to pattern match for here. Not having an else if (the list of pairs), and having an else if statement. The way the html works means
-- there can't be cases where nothing is input, as the default for anything to fill in for values or variables will have something (e.g. 0).
-- If there isn't an else if, then the expression is evaluated. The expression is evaluated using the eval function defined later on, and the result passed 
-- into an anonymous function which is then checked to see if it equals 0. When evaluating, if the statement evaluated is true, then 1 is returned, else 0 is returned.
-- Hence, \a not equalling 0 means it is true, and the relevant statements can be interpreted accordingly. Else, the other statements passed in can be evaluated, since we
-- know there are only 2 states the result of evaluating the expression can be. Now, for the case of there being an else if statement, we can just call the execution of an IfStmt
-- again where the else is, since the same thing will be done. The else if statement for this call will be the tail of the else if statement for the argument originally passed in,
-- since there can be multiple else if statements. This means the check for the empty list here acts as the edge case for this recursive loop. 
-- For the RepeatStmt, repeat will act like a for loop for imperative programming. The idea is the value of the evaluated expression will decrement with each function call, and
-- the loops will stop when less than or equal to 1 - i.e. the edge case. This uses do-notation. It evaluates the expression and interprets the statements passed in. It then checks what the value is,
-- and if it's less than or equal to 1, then the memory will be returned as it is, else, repeat will be called recursively, updating the value to decrement each time so there will be an end
-- to the recursive calls. 
current :: Stmt -> Memory -> Either Err Memory
current (AssignStmt var ex) mem = editMemory var ex mem
current (IfStmt x t [] f) mem =  eval x mem>>= \a -> if a /= 0 then interpret t mem else interpret f mem
--current (IfStmt x t [(ex, e)] f) mem = eval x mem>>= \a -> if a /= 0 then interpret t mem else eval ex mem >>= \b -> if b /= 0 then interpret e mem else interpret f mem
current (IfStmt x t ((ex, e):es) f) mem = eval x mem >>= \a -> if a /= 0 then interpret t mem else current (IfStmt ex e es f) mem
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

<<<<<<< HEAD
-- | This is called when assigning variables, and is called when there is an AssignStmt. This checks the memory to see if there is already a variable with that name to update.
-- There are 2 cases for evaluating the expression passed in, evaluating to Nothing - which will just add the value to the start of the memory, or Just "something" which means there is a value to update.
-- When there is a value to update, there is a replaceMemory function which is called to update the value accordingly. Lookup is used to check if there is the value we are looking for in memory - an inbuilt function.
=======
-- | This helper function is used when variable assignment takes place, and updates the
-- memory accordingly. If the variable already exists in memory, that value is updated,
-- else, it is added to the beginning of the memory store - more efficient to use cons
-- rather than ++, which runs through the whole list to add to the end.
>>>>>>> ac3738fdcadd78475b241a8334b4600e7fcba854
editMemory :: String -> Expr -> Memory -> Either Err Memory
editMemory s x mem = eval x mem >>= \val -> case lookup s mem of -- BINDING GETS RID OF EITHER
     Nothing -> Right $ (s, val):mem
     Just _ -> Right $ replaceMemory s val mem

<<<<<<< HEAD
-- | This is used to update the value in memory. The pattern matching for an empty memory will just return an empty memory since there isn't anything to update. Else, the memory is ran through and each fst of each pair
-- is checked. If the element equals what we are searching for, the pair is updated. Else, it is looped through until either the element is found, or we are left with an empty list which will just return an empty list.
=======
-- | This is called when there is a value in memory with that variable name already.
-- It updates the pair value stored in memory, and keeps it in the same position as
-- it was.
>>>>>>> ac3738fdcadd78475b241a8334b4600e7fcba854
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

-- EXTENSION IDEAS
-- RANDOM NUMBER GENERATOR
-- WHILE LOOP
-- AND, OR, NOT

--------------------------------------------------------------------------------
