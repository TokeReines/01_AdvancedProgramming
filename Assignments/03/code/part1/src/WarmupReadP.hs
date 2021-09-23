module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
-- E ::= T E' | E' | T  
-- E' :== "+" T E' | "-" T E' | e
-- T ::= num | "(" E ")"

-- 2 + 2 = E(T(2), E'("+", T(num), E'(e)) )
-- 1+23-(-456) = E(T(1), E'("+", T(23), E'("-", T("(", E("-", T(456)), ")"), e))
-- -1+23-(-456) = E'("-", T(1), E'("+", T(23), E'("-", T("(", E(E'("-", T(456), E'(e))), ")"))))


-- 2 + 2 = E(T(2)), 
-- "-1 + 23 - (- 456)"
-- Add (Add (Negate (Num 1))
--          (Num 23))
--     (Negate (Negate (Num 456)))

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

parseString :: String -> Either ParseError Exp
parseString s =
  case readP_to_S(do a <- pE; eof; return a) s of
    [] -> Left "cannot parse"
    [(a,_)] -> Right a -- the_must be "", since 'eof' ok_-> error"oops, my grammar is ambiguous!
    _ -> error "You've made an oopsie"

-- E ::= T E' | E' | T  
pE :: Parser Exp
pE = do e <- pT; pE' e;
     <|> do symbol '-'; e <- pT; pE' (Negate e);

-- E' :== "+" T E' | "-" T E' | e
pE' :: Exp -> Parser Exp
pE' e1 =  do ao <- pAddOp; e2 <- pT; pE' (ao e1 e2)
          <|>
          do no <- pNegOp; e2 <- pT; pE' (Add e1 (no e2))
          <|> return e1

-- T ::= num | "(" E ")"
pT :: Parser Exp
pT = do pNum;
     <|>
     do symbol '('; e <- pE; symbol ')'; return e


pNum :: Parser Exp
pNum =  lexeme $ do ds <- munch1 isDigit; return $ Num (read ds)

pAddOp :: Parser (Exp -> Exp -> Exp)
pAddOp = lexeme $ do symbol '+'; return Add

pNegOp :: Parser (Exp -> Exp)
pNegOp = lexeme $ do symbol '-'; return Negate

symbol :: Char -> Parser ()
symbol s = lexeme $ do satisfy(s ==); return ()

whitespace :: Parser ()
whitespace = do munch isSpace; return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

-- E ::= T E' | E' | T  
-- pE :: Parser Exp
-- pE = 
--     do t <- pT; e' <- pE'; 
--     <|> 
--     do e' <- pE';
--     <|>
--     do t <- pT;

-- -- E' :== "+" T E' | "-" T E' | e
-- pE' :: Parser Exp
-- pE' = 
--     do ao <- pAdd; t <- pT; e' <- pE'; return $ Add t e' 
--     <|>
--     do satisfy isMinus; t <- pT; e' <- pE'; return $ Negate (Add t e')
--     <|>
--     do return Num 0
-- -- T ::= num | "(" E ")"
-- pT :: Parser Exp
-- pT = 
--     do n <- pNum; return $ Num n 
--     <|>
--     do symbol '('; e <- pE; symbol ')'; return e

-- pAdd :: Parser (Exp -> Exp -> Exp)
-- pAdd = 

-- symbol 
-- symbol c = \s -> satisfy s == c

-- isPlus :: Char -> Bool
-- isPlus c = c == '+'

-- isMinus :: Char -> Bool
-- isMinus c = c == '-'

