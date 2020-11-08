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


-- main program prompting user to enter an expression
-- for the program to recognize.
mainRecognize = do
  putStrLn "Enter a legal expression"
  inputString <- getLine
  putStrLn (show (recognizeStr inputString))

------------------------------------------------------
-- THE PARSER
------------------------------------------------------
data Exp = A | AST_Tuple [Exp] | AST_Error String deriving (Eq,Show)

parseExp :: [Tokens] -> (Exp, [Tokens])
parseExp (AToken : rest) = (A,rest)
parseExp (LParen : rest) = 
    let (exp, rest2) = parseTuple rest 
      in if (head rest2) == RParen then (AST_Tuple exp, tail rest2)
         else (AST_Error "no closing parentheses",[])
-- error cases
parseExp ([EOF]) = (AST_Error "Unexpected end of input",[])
parseExp _ = (AST_Error "Fatal error in parseExp",[])



parseTuple :: [Tokens] -> ([Exp], [Tokens])
parseTuple (LParen : rest) = 
    let (exp, rest2) = parseExp(LParen : rest)
    in parseExpTail([exp], rest2)

parseTuple (AToken : rest) = 
    let (expression, rest2) = parseExp(AToken : rest)
    in parseExpTail([expression], rest2)
--error cases
parseTuple ([EOF]) = ([AST_Error "Unexpected end of input"],[])
parseTuple _ = ([AST_Error "Fatal error in parseAST_Tuple"],[])


parseExpTail :: ([Exp], [Tokens]) -> ([Exp], [Tokens])
parseExpTail (left, Comma : rest) = 
    let (right, rest2) = parseExp rest
    in parseExpTail(left ++ [right], rest2)
parseExpTail(left, RParen : rest) = (left, RParen : rest)
-- error case
parseExpTail _ = ([AST_Error "Fatal error in parseExpTail"],[]);

parse :: [Char] -> Exp
parse str = 
    let (ast,rest) = parseExp (getTokens str)
    in if (rest == [EOF]) then ast else (AST_Error "More input then expected.")



-- main program prompting user to enter an expression
-- for the program to recognize.
mainParser = do
  putStrLn "Enter a legal expression"
  inputString <- getLine
  putStrLn (show (parse inputString))





























