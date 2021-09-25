-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
-- add any other other imports you need
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP
import Data.Char

type Parser a = ReadP a

type ParseError = String -- you may replace this

parseString :: String -> Either ParseError Program
parseString s = case readP_to_S(do whitespace; a <- pProgram; eof; return a) s of 
                                [] -> Left "cannot parse"
                                [(a,_)] -> Right a
                                _ -> error "You've made an oopsie"

pProgram :: Parser Program
pProgram = do stmts <- pStmts;  return stmts;

pStmts :: Parser [Stmt] -- or Parser Program. ! Unsure about the order of these
pStmts = do stmt <- pStmt; return [stmt]
         (<|>)
         do stmt <- pStmt; symbol ';'; stmts <- pStmts; return (stmt : stmts); 

pStmt :: Parser Stmt
pStmt = 
        -- do i <- pI; symbol '='; e <- pExp; return (SDef i e)
        -- <|>
        do e <- pExp; return (SExp e)

pExp :: Parser Exp 
pExp = do n <- pNum; return (Const (IntVal n))
       <|>
       do s <- pStr; return (Const (StringVal s))

pOper :: Parser Op
pOper = undefined 

pCCz :: Parser CClause 
pCCz = undefined 

pCCi :: Parser Exp
pCCi = undefined 

pCCf :: Parser (String -> Exp)
pCCf = undefined 

pIdent :: Parser String -- Or VName or FName maybe in an either Monad?
pIdent = undefined

pNum :: Parser Int
pNum = lexeme $ do symbol '-';  n <- pNum; return (-n)
       <|>
       do ds <- munch1 isDigit;
          case ds of
            [] -> pfail
            [d] ->  return (ord d - ord '0')
            (d:ds') ->  if d == '0' then pfail else return (read (d:ds'))

pStr :: Parser String
pStr = do 
    symbol '\''
    s <- many character;
    -- s <- between (char '\'') (char '\'') (many (escaped <|> normalChar))
    symbol '\''; 
    return concat s  

-- pStr = do symbol '\''; s <- pStr; return s;
--        <|>


-- Helper functions
noneEscape :: Parser String
noneEscape = oneOf "\\\"\n";

character :: Parser String
character = try (string "\\\\") <|> try (string "\\\"")

escape :: Parser String
escape = do
    e <- char '\\'
    c <- oneOf "n\"\\"
    return [e, c]
        
reservedIdents = ["None", "True", "False", "for", "if", "in", "not"]

oneOf :: [Char] -> ReadP Char
oneOf cs = choice [char c | c <- cs]

symbol :: Char -> Parser ()
symbol s = lexeme $ do satisfy(s ==); return ()


whitespace :: Parser ()
whitespace = do munch isSpace; return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a