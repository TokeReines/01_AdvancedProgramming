module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
-- E ::= T E' | "-" T E'
-- E' :== "+" T E' | "-" T E' | E
-- T ::= num | "(" E ")"

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
  case readP_to_S(do whitespace; a <- pE; eof; return a) s of
    [] -> Left "cannot parse"
    [(a,_)] -> Right a -- the_must be "", since 'eof' ok_-> error"oops, my grammar is ambiguous!
    _ -> error "You've made an oopsie"

-- E ::= T E' | "-" T E'
pE :: Parser Exp
pE = do e <- pT; pE' e;
     <|> do symbol '-'; e <- pT; pE' (Negate e);

-- E' :== "+" T E' | "-" T E' | E
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

resultOfString = Right (Add (Add (Negate (Num 1)) (Num 23)) (Negate (Negate (Num 456))))
testParseString =  parseString "-1+23-(-456)" == resultOfString
                && parseString "-1" == Right (Negate (Num 1))
                && parseString "1" == Right (Num 1)
                && parseString "1+2" == Right (Add (Num 1 ) (Num 2))
                && parseString "-1+2" == Right (Add (Negate (Num 1 )) (Num 2))
                && parseString "1-2" == Right (Add (Num 1) (Negate (Num 2)))
                && parseString "1-(-(-(-1)))" == Right (Add (Num 1) (Negate (Negate (Negate (Negate (Num 1))))))
                && parseString " - 1 + 23 - ( - 456 ) " == resultOfString
                && parseString "   -   1   +   23   -   (   -   456   ) " == resultOfString
                && parseString "  \
                \-   1\t   +\n   23   -   (   -   456   ) " == resultOfString
