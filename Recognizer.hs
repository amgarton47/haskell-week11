module Recognizer where

---------------------------------------------------
-- THE RECOGNIZER
---------------------------------------------------
-- main program prompting user to enter an expression
-- for the program to recognize.
main = do
  putStrLn "Enter a legal expression"
  inputString <- getLine
  putStrLn (show (recognizeStr inputString))

-- helper functions to recognize tokens
isA AToken = True
isA _ = False
isComma Comma = True
isComma _ = False
isLParen LParen = True
isLParen _ = False
isRParen RParen = True
isRParen _ = False

{- Recognizes an expression.  If an expression is found, it returns the input
   remaining after the expression.  If an expression is not found, it
   throws an error exception.
-}
recognizeExp :: [Tokens] -> [Tokens]
recognizeExp (AToken:rest) = rest
recognizeExp (LParen:rest) = 
  let
    rest2 = recognizeTuple rest
  in if head rest2 == RParen then (tail rest2)
     else error "no closing parenthesis in Exp"
-- error cases
recognizeExp (EOF:rest) = error "Unexpected end of input"
recognizeExp _ = error "Fatal error in Exp"

{- Recognizes a tuple.  If a tuple is found, returns the input
   remaining after the tuple.  If a tuple is not found, it
   throws an error exception.
-}
recognizeTuple :: [Tokens] -> [Tokens]
recognizeTuple tokens@(first:rest)
  | (isA first || isLParen first) = recognizeExpTail(recognizeExp tokens)
  -- error cases
  | (first) == EOF = error "Unexpected end of input"
  | otherwise  = error "Fatal error in Tuple"

{- Recognizes the tail of an expression.  Finds the longest ExpTail
   and returns the remaining tokens.  If a ExpTail is not found, it
   throws an error exception.
-}
recognizeExpTail :: [Tokens] -> [Tokens]
recognizeExpTail tokens@(first:rest)
  | isRParen first = tokens
  | isComma first = recognizeExpTail(recognizeExp rest)
  | (first == EOF) = error "Unexpected end of input"
  | otherwise  = error "Fatal error in ExpTail"


-- Return True if the list of tokens passed in comprises a valid expression.
-- Otherwise it throws an error exception.
recognize :: [Tokens] -> Bool
recognize tokens =
  let
    rest = recognizeExp tokens
  in
    if rest == [EOF] || rest == [] then
      True
    else
      error "More input than expected."

-- Returns True if the provided string is a valid expression
recognizeStr :: [Char] -> Bool
recognizeStr str = recognize (getTokens str)
