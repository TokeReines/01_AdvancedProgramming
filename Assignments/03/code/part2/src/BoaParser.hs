-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
-- add any other other imports you need
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char

type Parser a = ReadP a

type ParseError = String -- you may replace this

parseString :: String -> Either ParseError Program
parseString s = case readP_to_S(do whitespace; a <- pP; eof; return a) s of 
                                [] -> Left "cannot parse"
                                [(a,_)] -> Right a
                                _ -> error "You've made an oopsie"

pP :: Parser Program
pP = do stmts <- pStmts;  return stmts;

pStmts :: Parser [Stmt] -- or Parser Program. ! Unsure about the order of these
pStmts = do stmt <- pStmt; return [stmt]
         <|> 
         do stmt <- pStmt; symbol ';'; stmts <- pStmts; return (stmt : stmts); 

pStmt :: Parser Stmt
pStmt = do i <- pI; symbol '='; e <- pExp; return (SDef i e)
        <|>
        do e <- pExp; return (SExp e)

pExp :: Parser Exp 
pExp = do n <- pNum; return (Const (IntVal n))

pOp :: Parser Op
pOp = undefined 

pCCz :: Parser CClause 
pCCz = undefined 

pCCi :: Parser Exp
pCCi = undefined 

pCCf :: Parser (VName -> Exp)
pCCf = undefined 

pI :: Parser VName
pI = undefined

pNum :: Parser Int
pNum = do symbol '-';  n <- pNum; return (-n)
       <|>
       do symbol '0'; symbol '0'; pfail;
       <|>
       do ds <- munch1 isDigit; return (read ds)


pStr :: Parser String
pStr = undefined 



-- Helper functions

symbol :: Char -> Parser ()
symbol s = lexeme $ do satisfy(s ==); return ()

whitespace :: Parser ()
whitespace = do munch isSpace; return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a