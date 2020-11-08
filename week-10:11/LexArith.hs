module LexArith (getTokens, Token(ID, NUM, Plus, Minus, Mult, Div, Neg, 
                            LParen, RParen, EOF, Error)) where

import Data.Char
main = do
          putStrLn "Enter a legal arithmetic expression"
          inputString <- getLine
          putStrLn (show (getTokens inputString))

data Token = ID String | NUM Int | Plus | Minus | Mult | Div | Neg
           | LParen | RParen | EOF | Error String deriving (Eq,Show)


-- Because imported Data.Char above, don't need to write
-- Data.Char.isAlpha, etc.

-- Return true if c is a letter or digit 
alphanum c = (isAlpha c) || (isDigit c)

{- Extracts consecutive alphanumeric characters from the input to
   build up an identifier.  Returns a tuple containing the next
   identifier in the input and the input left over after removing
   the identifier.  (Very inefficient!)
       Precondition:  The initial character of the identifier has
         already been found and is passed in in the second parameter.
       Parameter 1:  Input to extract the identifier from
       Parameter 2:  The characters found so far in the identifier. -}
getid :: [Char] -> [Char] -> ([Char], [Char])    
getid [] id = (id, [])
getid s@(c:rest) id = 
                if (alphanum c) then getid rest (id ++[c])
                else (id, s)

{- Extracts consecutive digits from the input to
       build up an integer.  Returns a tuple containing the next
       integer in the input and the input left over after removing
       the integer.
       Precondition:  The initial digit of the integer has
         already been found and is passed in in the second parameter.
       Parameter 1:  Input to extract the integer from
       Parameter 2:  The digits found so far in the integer. -}
getnum :: [Char] -> Int -> (Int, [Char])
getnum [] num = (num, [])
getnum s@(c:rest) num =
                if (isDigit c) then 
                    getnum rest (num*10 + ((ord c)-ord '0'))
                else (num, s) 

-- Return the next token found in the input paired with the rest of the input 
getToken [] = (EOF,[])
getToken  ('+':rest) = (Plus, rest)
getToken ('-':rest) = (Minus, rest)
getToken ('*':rest) = (Mult, rest)
getToken ('/':rest) = (Div, rest)
getToken ('~':rest) = (Neg, rest)  -- Use ~ as unary negation
getToken ('(':rest) = (LParen, rest)
getToken (')':rest) = (RParen, rest)
getToken (c:rest) =      --trickier cases!
          if isSpace c then
            -- Recurse to skip white space 
            getToken rest
          else if isAlpha c then 
            -- Return identifier 
            let
              (id, remainder) = (getid rest [c])
            in
              (ID id, remainder)
          else if (isDigit c) then
           -- Return number 
            let
              (num, remainder) = getnum rest ((ord c) - (ord '0'))
            in
              (NUM num, remainder)
          else
            (Error( "Skipping illegal character "++[c]++"."), rest) 

-- Return the list of tokens found in the input.
-- Parameter:  A character list to tokenize
    
getTokens list = let
        (token,rest) = getToken list
    in
        if token == EOF 
           then [EOF]
           else token:(getTokens rest)

