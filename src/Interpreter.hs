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
    | IncorrectExpression               -- ^ Tried applying an expression 
                                        -- that shouldn't be applied.
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Given a @program@ and @memory@, each statement in the program is evaluated using a helper
-- function, @current@. The empty list pattern match acts as an edge case for the recursive binding,
-- and also as a check for there being no memory to begin with. 
-- The result  of @current p mem@ is passed in to the @interpret ps@.
interpret :: Program -> Memory -> Either Err Memory
interpret [] mem = Right mem
interpret (p:ps) mem = current p mem >>= interpret ps


-- | Given a @statement@ and @memory@, this function outputs the new updated contents of memory to pass back into interpret.
current :: Stmt -> Memory -> Either Err Memory
-- | Given an AssignStmt as a Stmt, the var name passed in will be assigned the expression.
-- Evaluating these using editMemory will add the variable and corresponding value to the memory.

-- >>> AssignStmt String Expr
current (AssignStmt var ex) mem = editMemory var ex mem
-- >>> current (IfStmt x t [] f) mem =  eval x mem>>= \a -> if a /= 0 then interpret t mem else interpret f mem
-- Above is a previous version of my code for this pattern match, which makes use of bindings rather than do-notation.
-- This is to improve readability of the code.

-- | Given an @IfStmt@, the expression provided is evaluated and the relevant statements are evaluated
-- following the results of the expression.
-- The empty list can act as an edge case for the recursive call, as well as just in general if
-- an else if isn't provided. When there is an else if provided, we evaluate each pair in the
-- list until we reach a statement which is true or reach the end of the list. 
-- >>> IfStmt Expr [Stmt] [Stmt] [Stmt]
current (IfStmt x t [] f) mem = do
     a <- eval x mem
     if a /= 0 then interpret t mem
     else interpret f mem
current (IfStmt x t ((ex, e):es) f) mem = do
     a <- eval x mem
     if a /= 0 then interpret t mem
     else current (IfStmt ex e es f) mem
-- | Given a @RepeatStmt@, the expression provided is evaluated and the statements are evaluated
-- provided the expression evaluation doesn't meet the requirements of the if statement.
-- If @val@ is less than or equal to 1, this acts as an edge case to stop the loop, 
-- since it will have then repeated the required number of times - this acts similarly
-- to a for loop in imperative languages.
-- >>> RepeatStmt Expr [Stmt]
current (RepeatStmt ex xs) mem = do
     val <- eval ex mem
     m <- interpret xs mem
     if val <= 1 then Right m
     else current (RepeatStmt (ValE $ val-1) xs) m

current (WhileStmt (ValE _) _) _ = Left IncorrectExpression
current (WhileStmt (VarE _) _) _ = Left IncorrectExpression
current (WhileStmt x xs) mem = do
     expr <- eval x mem
     m <- interpret xs mem
     if expr == 1 then current (WhileStmt x xs) m
     else Right m

-- | Given a @string@, @expr@ and @memory@, the @memory@ is searched to
-- see if the given variable name already exists. This makes use of lookup, which
-- is an inbuilt function that returns a Maybe. If something is found, then
-- @replaceMemory@ is called, which then updates the value. If the variable
-- doesn't already exist, then it is added to the head of the memory
-- for efficiency, as opposed to adding to the end of the list.
editMemory :: String -> Expr -> Memory -> Either Err Memory
editMemory s x mem = eval x mem >>= \val -> case lookup s mem of -- BINDING GETS RID OF EITHER
     Nothing -> Right $ (s, val):mem
     Just _ -> Right $ replaceMemory s val mem

-- | Given a @string@ and @integer@, the variable is found in memory and replaced.
-- We account for there being an empty list passed in for good practice and in case there 
-- was a mistake somewhere with the passing of arguments. It also acts as an edge case for
-- the recursive call, which is used to run through the memory passed in.
replaceMemory :: String -> Int -> Memory -> Memory
replaceMemory _ _ [] = []
replaceMemory s i (m:ms)
    | fst m == s = (s, i):ms
    | otherwise = m:replaceMemory s i ms

-- | Given an @expr@ and @memory@, evaluates the expression and returns
-- the result, while also accounting for errors.
eval :: Expr -> Memory -> Either Err Int
-- | In the case of the expression being a ValE (i.e. a value), the
-- result is just the integer the value holds.
eval (ValE x) _ = Right x
-- | Given a @VarE@, the name is looked up in memory. If there is a value that can be associated
-- with that name, it is returned. Else, there is an uninitialisedmemory error, since the variable
-- doesn't exist.
eval (VarE x) mem = case lookup x mem of
     Nothing -> Left $ UninitialisedMemory x
     Just y -> Right y
-- | Given a @BinOpE@, the expressions passed in are evaluated together using the operator.
-- @subEval@ is used to perform arithmetic operations, such as +, -, *.
-- @subEvalBin@ is used when there are binary operations involved, and only one of two values
-- can be returned.
-- @Div@ and @Pow@ are evaluated here due to their return type.
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

eval (BitOpE f a b) mem = do
     x <- eval a mem
     y <- eval b mem
     evalBin f x y

-- | Given a function and two expressions, evaluates the expressions and returns the result.
subEval :: (Int -> Int -> Int) -> Expr -> Expr -> Memory -> Either Err Int
subEval f a b mem = do
     x <- eval a mem
     y <- eval b mem
     return (f x y)

-- | Given a function and two expressions, evaluates the expressions and returns the result.
subEvalBin :: (Int -> Int -> Bool) -> Expr -> Expr -> Memory -> Either Err Int
subEvalBin f a b mem = do
     x <- eval a mem
     y <- eval b mem
     return (if f x y then 1 else 0)

evalBin :: BitWise -> Int -> Int -> Either Err Int
evalBin And a b = if a /= 0 && b /= 0 then Right 1 else Right 0
evalBin Or a b = if a /= 0 || b /= 0 then Right 1 else Right 0
-- WRITE TESTS!!!!!!!!!!
     
{-evalBin :: (Int -> Int -> Bool) -> Expr -> Expr -> Memory -> Bool 
evalBin f x y mem = do
     a <- eval x mem
     b <- eval y mem
     f a b-}

-- TRYING TO CHANGE UP TO MAKE EASIER TO IMPLEMENT AND, OR AND NOT

--subEvalBit :: (Int -> Int -> Bool) -> Expr -> Expr -> Memory -> Either Err Int
--subEvalBit And x y mem = 

-- | This function ensures there is no accidental divide by zero error. The DivBZeroError is returned
-- when this occurs, and the program handles it correctly.
safediv :: Int -> Int -> Either Err Int
safediv _ 0 = Left DivByZeroError
safediv x y = Right $ x `div` y

-- EXTENSION IDEAS
-- RANDOM NUMBER GENERATOR
-- WHILE LOOP
-- REPEAT UNTIL
-- AND, OR, NOT
-- MODULUS


--------------------------------------------------------------------------------
