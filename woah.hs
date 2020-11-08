data Tokens = AToken | LParen | RParen | Comma | Error String | EOF deriving (Eq,Show)

getTokens :: [Char] -> [Tokens]
getTokens [] = [EOF]
getTokens ('a' : rest) = (AToken :  getTokens rest)
getTokens ('(' : rest) = (LParen :  getTokens rest)
getTokens (')' : rest) = (RParen :  getTokens rest)
getTokens (',' : rest) = (Comma  :  getTokens rest)
getTokens (c   : rest) = [Error "Illegal expression"]



data Exp = A | AST_Tuple [Exp] | AST_Error String deriving (Eq,Show)

parseExp :: [Tokens] -> (Exp, [Tokens])
parseExp (AToken:rest) = (A,rest)
parseExp (LParen:rest) = 
    let (expTuple,afterParen) = parseTuple rest 
    in if (head afterParen) == RParen then (AST_Tuple expTuple,tail afterParen) 
        else (AST_Error "no closing parentheses",[])
parseExp ([EOF]) = (AST_Error "Unexpected end of input",[])
parseExp _ = (AST_Error "Fatal error in parseExp",[])


parseTuple :: [Tokens] -> ([Exp], [Tokens])
parseTuple (LParen:rest) = 
    let (expression,others) = parseExp(LParen:rest)
    in parseExpTail([expression],others)
parseTuple (AToken:rest) = 
    let (expression,others) = parseExp(AToken:rest)
    in parseExpTail([expression],others)
parseTuple ([EOF]) = ([AST_Error "Unexpected end of input"],[])
parseTuple _ = ([AST_Error "Fatal error in parseAST_Tuple"],[])


parseExpTail :: ([Exp], [Tokens]) -> ([Exp], [Tokens])
parseExpTail (left,Comma:others) = 
    let (right,rest) = parseExp others
    in parseExpTail(left++[right], rest)
parseExpTail(left,RParen:others) = (left,RParen:others)
parseExpTail _ = ([AST_Error "Fatal error in parseExpTail"],[]);


parse :: [Char] -> Exp
parse str = 
	let (ast,rest) = parseExp (getTokens str)
	in if (rest == [EOF]) then ast else (AST_Error "extra tokens left at end")