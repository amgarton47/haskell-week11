-- Week 10/11 haskell assignment
-- Aidan Garton & Jett Bronstein

module Submission where

-- main program prompting user to enter an expression
-- for the program to tokenize.
mainLexer = do
    putStrLn "Hello, this is our lexer!" 
    putStrLn "Enter a legal expression"
    inputString <- getLine
    putStrLn (show (getTokens inputString))

-- main program prompting user to enter an expression
-- for the program to parse.
mainParser = do
  putStrLn "Enter a legal expression"
  inputString <- getLine
  putStrLn (show (parse inputString))

---------------------------------------------------
-- THE LEXER (1.b)
---------------------------------------------------
-- liting of all types to be tokenized
data Tokens = AToken | LParen | RParen | Comma | Error String | EOF deriving (Eq,Show)

-- tokenizes an input string for the alphabet {"a", "(", ")", ",")}
getTokens :: [Char] -> [Tokens]
getTokens [] = [EOF]
getTokens ('a' : rest) = (AToken :  getTokens rest)
getTokens ('(' : rest) = (LParen :  getTokens rest)
getTokens (')' : rest) = (RParen :  getTokens rest)
getTokens (',' : rest) = (Comma  :  getTokens rest)
-- ignore whitespace
getTokens (' ' : rest) = getTokens rest
-- error case for symbols not in the alphabet
getTokens (c   : rest) = [Error "Illegal expression"]

------------------------------------------------------
-- THE PARSER (1.d)
------------------------------------------------------
-- datatype representation of an abstract syntax tree for an 
-- expression in the grammar defined in the hw
data Exp = A | AST_Tuple [Exp] | AST_Error String deriving (Eq,Show)

{- Parses an expression. If an expression is found, returns a
   tuple containing the ast for the expression and the input
   following the expression. If an expression is not found, it
   returns an error and consumes all remaining input.
-}
parseExp :: [Tokens] -> (Exp, [Tokens])
parseExp (AToken : rest) = (A,rest)
parseExp (LParen : rest) = 
    let (exp, rest2) = parseTuple rest 
      in if (head rest2) == RParen then (AST_Tuple exp, tail rest2)
         else (AST_Error "no closing parentheses",[])
-- error cases
parseExp ([EOF]) = (AST_Error "Unexpected end of input",[])
parseExp _ = (AST_Error "Fatal error in parseExp",[])

{- Parses a tuple.  If a tuple is found, returns a
   tuple containing the ast for the tuple and the input
   following the factor.  If a tuple is not found, it
   returns an error and consumes all remaining input.
-}
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

{- Parses the tail of an expression.  Finds the longest ExpTail
   and returns a tuple of the parse tree found so far and
   the input remaining.  If a ExpTail is not found, it
   returns an error and consumes all remaining input.
-}
parseExpTail :: ([Exp], [Tokens]) -> ([Exp], [Tokens])
parseExpTail (left, Comma : rest) = 
    let (right, rest2) = parseExp rest
    in parseExpTail(left ++ [right], rest2)
parseExpTail(left, RParen : rest) = (left, RParen : rest)
-- error case
parseExpTail _ = ([AST_Error "Fatal error in parseExpTail"],[]);

-- Return an AST for the list of tokens passed in. 
parse :: [Char] -> Exp
parse str = 
    let (ast,rest) = parseExp (getTokens str)
    in if (rest == [EOF]) then ast else (AST_Error "More input then expected.")
    