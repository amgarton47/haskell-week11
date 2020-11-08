-- Week 10/11 haskell assignment
-- Aidan Garton & Jett Bronstein

module Lexer where

main = do
    putStrLn "Hello, this is our lexer!" 
    putStrLn "Enter a legal expression"
    inputString <- getLine
    putStrLn (show (getTokens inputString))

data Tokens = AToken | LParen | RParen | Comma | Error String | EOF deriving (Eq,Show)

-- 1.b.
-- THE LEXER
-- tokenizes an input string for the alphabet {"a", "(", "}"", ",")}
getTokens :: [Char] -> [Tokens]
getTokens [] = [EOF]
getTokens ('a' : rest) = (AToken :  getTokens rest)
getTokens ('(' : rest) = (LParen :  getTokens rest)
getTokens (')' : rest) = (RParen :  getTokens rest)
getTokens (',' : rest) = (Comma  :  getTokens rest)
getTokens (c   : rest) = [Error "Illegal expression"]


-- THE RECOGNIZER
------------------------------------------------
-- helper functions to recognize tokens
isA AToken = True
isA _ = False

isComma Comma = True
isComma _ = False

isLParen LParen = True
isLParen _ = False

isRParen RParen = True
isRParen _ = False

-- recognizeExp :: [Tokens] -> [Tokens]
-- recognizeExp tokens@(first:rest) 
--   | isA      first = rest
--   | isLParen first = recognizeTuple 
--   | (first == EOF) = error "Unexpected end of input"
--   | otherwise  = error "Fatal error in Expression"

recognizeExp :: [Tokens] -> [Tokens]
recognizeExp (AToken:rest) = rest
recognizeExp (LParen:rest) = 
  let
    rest2 = recognizeTuple rest
  in
    if head rest2 == RParen then (tail rest2)
    else error "no closing parenthesis in Exp"
-- error cases
recognizeExp (EOF:rest) = error "Unexpected end of input"
recognizeExp _ = error "Fatal error in Exp"

recognizeTuple :: [Tokens] -> [Tokens]
recognizeTuple tokens@(first:rest)
  | (isA first || isLParen first) = recognizeExpTail(recognizeExp tokens)
  -- error cases
  | (first) == EOF = error "Unexpected end of input"
  | otherwise  = error "Fatal error in Tuple"

recognizeExpTail :: [Tokens] -> [Tokens]
recognizeExpTail tokens@(first:rest)
  | isRParen first = tokens
  | isComma first = recognizeExpTail(recognizeExp rest)
  | (first == EOF) = error "Unexpected end of input"
  | otherwise  = error "Fatal error in ExpTail"


-- Return True if the list of tokens passed in comprises an expression.
-- Otherwise throw an error exception
recognize :: [Tokens] -> Bool
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

------------------------------------------------------
-- THE PARSER
------------------------------------------------------



