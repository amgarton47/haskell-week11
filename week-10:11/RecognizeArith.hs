module RecognizeArith(recognize,recognizeStr) where
-- Module that determines whether an expression is a legal arithmetic expression.

import LexArith  -- provides tokens and getTokens function

-- helper functions to recognize tokens
isID (ID v) = True
isID _ = False
isNum (NUM n) = True
isNum _ = False
isLParen LParen = True
isLParen _ = False
isRParen RParen = True
isRParen _ = False

-- The following helper functions are used instead of recognizeMulOp
-- and recognizeAddOp.

-- Helper function returning true only for * or /
isMulOp Mult = True
isMulOp Div = True
isMulOp _ = False

-- Helper function returning true only for + or -
isAddOp Plus = True
isAddOp Minus = True
isAddOp _ = False

{- Recognizes an expression.  If an expression is found, it returns the input
   remaining after the expression.  If an expression is not found, it
   throws an error exception.
-}
recognizeExp :: [Token] -> [Token]

             -- An expression derived by <exp> ::= <term> <termtail>    (1)
recognizeExp tokens@(fst:rest)
  | (isID fst || isNum fst || fst == LParen) =
      recognizeTermTail(recognizeTerm(tokens))
      -- error cases
  | (fst == EOF) = error "Unexpected end of input"
  | otherwise  = error "Fatal error in Expression"

{- Recognizes a term.  If a term is found, returns the input
   remaining after the factor.  If a term is not found, it
   throws an error exception.
-}
recognizeTerm :: [Token] -> [Token]
      -- A term derived by <term> ::= <factor> <factorTail>  (4)
recognizeTerm tokens@(fst:rest)
    | (isID fst || isNum fst || fst == LParen) =
                   recognizeFactorTail(recognizeFactor(tokens))
      -- error cases
    | fst == EOF = error "Unexpected end of input"
    | otherwise  = error "Fatal error in Term"

{- Recognizes a factor.  If a factor is found, it returns the
   remaining tokens following the factor.  If a factor is not found, it
   throws an error exception.
-}
recognizeFactor :: [Token] -> [Token]
recognizeFactor ((ID v) :rest) = rest   -- <factor> ::= ID      (9)
recognizeFactor ((NUM n): rest) = rest  -- <factor> ::= Num     (8)
recognizeFactor (LParen: rest) =        -- <factor> ::= (<exp>) (7)
  let
    rest2 =recognizeExp rest
  in
    if head rest2 == RParen then (tail rest2)
    else (error "no closing parenthesis in Factor")
         -- error cases
recognizeFactor (EOF: rest) = error "Unexpected end of input"
recognizeFactor _ = error "Fatal error in Factor"

{- Recognizes the tail of a term.  Finds the longest TermTail
   and returns the remaining tokens.  If a TermTail is not found, it
   throws an error exception.
-}
recognizeTermTail :: [Token] -> [Token]
      -- a termTail derived by <termTail> ::= <addop> <term> <termTail>  (2)
recognizeTermTail tokens@(fst:others)
    | isAddOp fst = recognizeTermTail(recognizeTerm others)
     -- a termTail derived from <termTail> ::= epsilon     (3)
    | (fst == RParen || fst == EOF) = tokens
    -- error as no entry in table for everything else!
    | otherwise = error "Fatal error in termTail"

{- Recognizes the tail of a factor.  Finds the longest FactorTail
   and returns the remaining tokens.  If a FactorTail is not found, it
   throws an error exception.
-}
recognizeFactorTail :: [Token] -> [Token]
      -- a factorTail derived by <factorTail> ::= <mulop> <factor> <factorTail> (5)
recognizeFactorTail tokens@(fst:others)
    | isMulOp fst = recognizeFactorTail(recognizeFactor others)
      -- a FactorTail derived by <factorTail> ::= epsilon    (6)
    | (isAddOp fst || fst == RParen || fst == EOF) = tokens
      -- error cases
    | otherwise = error "Fatal error in factorTail"

-- Return True if the list of tokens passed in comprises an expression.
-- Otherwise throw an error exception
recognize :: [Token] -> Bool
recognize tokens =
  let
    rest = recognizeExp tokens
  in
    if rest == [EOF] || rest == [] then
      True
    else
      error "More input than expected."

  -- Return True if the string passed in comprises an expression.
  -- Otherwise throw an error exception
recognizeStr :: [Char] -> Bool
recognizeStr str = recognize (getTokens str)


  -- main program prompting user to enter an arithmetic expression
  -- for the program to recognize.
mainRecognize = do
  putStrLn "Enter a legal arithmetic expression"
  inputString <- getLine
  putStrLn (show (recognizeStr inputString))
