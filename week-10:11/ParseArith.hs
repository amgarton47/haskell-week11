module ParseArith(parse,parsestr,ArithExp(AST_ID, AST_NUM, AST_NEG, AST_PLUS,
         AST_MINUS, AST_PRODUCT, AST_QUOTIENT, AST_ERROR)) where 

import LexArith

-- Abstract syntax trees for arithmetic expressions 
data ArithExp = AST_ID String | AST_NUM Int | 
         AST_NEG ArithExp | AST_PLUS (ArithExp, ArithExp) | 
         AST_MINUS (ArithExp, ArithExp) | AST_PRODUCT (ArithExp, ArithExp) | 
         AST_QUOTIENT (ArithExp, ArithExp) | AST_ERROR String deriving (Eq,Show)
    
{-  Now define the parsing part of the program, which takes a list of  
    tokens and returns an abstract syntax tree.                        -}

{- Parses an expression.  If an expression is found, returns a
   tuple containing the ast for the expression and the input
   following the expression.  If an expression is not found, it
   returns an error and consumes all remaining input.
-}
parseExpression :: [Token] -> (ArithExp, [Token])

-- Normal start to expression, <exp> ::= <term> <termtail>    (1)
parseExpression ((ID v):tl) = parseTermTail(parseTerm((ID v):tl))
parseExpression ((NUM n):tl) = parseTermTail(parseTerm((NUM n):tl))
parseExpression (LParen:tl) = parseTermTail(parseTerm(LParen:tl))

-- error cases 
parseExpression [EOF] =  (AST_ERROR "Unexpected end of input", [])
parseExpression _ = (AST_ERROR "Fatal error", [])

{- Parses a term.  If a term is found, returns a
   tuple containing the ast for the term and the input
   following the factor.  If a term is not found, it
   returns an error and consumes all remaining input.
-}
parseTerm :: [Token] -> (ArithExp, [Token])

-- Normal start to expression, <term> ::= <factor> <factorTail>  (4)
parseTerm ((ID v):tl) = parseFactorTail(parseFactor((ID v):tl))
parseTerm ((NUM n):tl)  = parseFactorTail(parseFactor((NUM n):tl))
parseTerm (LParen:tl)  = parseFactorTail(parseFactor(LParen:tl))

-- error cases 
parseTerm [EOF] = (AST_ERROR "Unexpected end of input", [])
parseTerm _ = (AST_ERROR "Fatal error", [])

{- Parses a factor.  If a factor is found, returns a
   tuple containing the ast for the factor and the input
   following the factor.  If a factor is not found, it
   returns an error and consumes all remaining input.
-}
parseFactor :: [Token] -> (ArithExp, [Token])

-- Normal start to factor, <factor> ::= ID      (9)
parseFactor ((ID v):rest) = (AST_ID v,rest)
-- <factor> ::= Num                             (8)
parseFactor ((NUM n):rest)  = (AST_NUM n,rest)
-- <factor> ::= (<exp>)                         (7)
parseFactor (LParen:rest)  = 
            let (eTree,rest2) = parseExpression rest
            in if head rest2 == RParen then (eTree,tail rest2) 
                  else (AST_ERROR "no closing parenthesis",[])

-- error cases 
parseFactor [EOF] = (AST_ERROR "Unexpected end of input", [])
parseFactor _ = (AST_ERROR "Fatal error", [])

{- Parses the tail of a term.  Finds the longest TermTail
   and returns a tuple of the parse tree found so far and
   the input remaining.  If a TermTail is not found, it
   returns an error and consumes all remaining input.
-}
parseTermTail :: (ArithExp, [Token]) -> (ArithExp, [Token])

-- Normal start to expression, <termTail> ::= <addop> <term> <termTail>  (2) 
parseTermTail (left, Plus:others) =
              let
                (right,rest) = parseTerm others
              in 
                parseTermTail(AST_PLUS (left,right), rest)

parseTermTail (left, Minus:others) = 
              let
                 (right,rest) = parseTerm others
              in 
                 parseTermTail(AST_MINUS (left,right), rest)

-- Legal tokens to follow TermTail / <termTail> ::= epsilon           (3)
parseTermTail (term, RParen:others) = (term, RParen:others)
parseTermTail (term, EOF:others) = (term, EOF:others)

-- error cases 
parseTermTail _ =  (AST_ERROR "Fatal error", [])

{- Parses the tail of a factor.  Finds the longest FactorTail
   and returns a tuple of the parse tree found so far and
   the input remaining.  If a FactorTail is not found, it
   returns an error and consumes all remaining input.
-}
parseFactorTail :: (ArithExp, [Token]) -> (ArithExp, [Token])

-- Normal start to expression, <factorTail> ::= <mulop> <factor> <factorTail> (5)
parseFactorTail (left, Mult:others) = 
                let
                   (right,rest) = parseFactor others
                in 
                   parseFactorTail(AST_PRODUCT(left,right), rest)

parseFactorTail (left, Div:others) = 
                let
                   (right,rest) = parseFactor others
                in 
                   parseFactorTail(AST_QUOTIENT (left,right), rest)

-- Legal tokens to follow FactorTail / <factorTail> ::= epsilon           (6)
parseFactorTail (term, Plus:others) = (term, Plus:others)
parseFactorTail (term, Minus:others) = (term, Minus:others)
parseFactorTail (term, RParen:others) = (term, RParen:others)
parseFactorTail (term, EOF:others) = (term, EOF:others)

-- error cases 
parseFactorTail _ = (AST_ERROR "Fatal error", [])



-- Return an AST for the list of tokens passed in. 
parse :: [Token] -> ArithExp
parse tokens =
      let 
         (ast1, rest) = parseExpression tokens
      in
         if rest == [EOF] || rest == [] then
            ast1
         else
           AST_ERROR "More input than expected."

parsestr :: [Char] -> ArithExp
parsestr str = parse (getTokens str)

mainParse = do
          putStrLn "Enter a legal arithmetic expression"
          inputString <- getLine
          putStrLn (show (parsestr inputString))
