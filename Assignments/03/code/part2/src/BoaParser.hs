-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
-- add any other other imports you need

import Data.Char
import Text.ParserCombinators.Parsec

parseString :: String -> Either ParseError Program
parseString = parse (do spaces; a <- pProgram; eof; return a) ""

-- Main Parsers

pProgram :: Parser Program
pProgram = do pStmts

pStmts :: Parser [Stmt]
pStmts =
  try (do stmt <- pStmt; symbol ';'; stmts <- pStmts; return (stmt : stmts))
    <|> do stmt <- pStmt; return [stmt]

pStmt :: Parser Stmt
pStmt =
  try (do i <- pIdent; spaces; symbol '='; SDef i <$> pExp)
    <|> do SExp <$> pExp

pExp :: Parser Exp
pExp =
  lexeme $
    pComments $
      try (do e1 <- pExp'; spaces; ro <- pRelNegOp; spaces; Not . ro e1 <$> pExp')
        <|> try (do e1 <- pExp'; spaces; ro <- pRelOp; spaces; ro e1 <$> pExp')
        <|> pExp'

pExp' :: Parser Exp
pExp' =
  do pOper
    <|> do pExp''

pExp'' :: Parser Exp
pExp'' =
  do pNum
    <|> do Const . StringVal <$> pStr
    <|> do Const <$> pNoneTrueFalse
    <|> try (do i <- pIdent; spaces; ez <- between (symbol '(') (symbol ')') pExpz; return $ Call i ez)
    <|> try (do Var <$> pIdent)
    <|> try (do string "not"; spaces; Not <$> pExp)
    <|> do between (symbol '(') (symbol ')') pExp
    <|> try (do symbol '['; e <- pExp; spaces; fc <- pForC; spaces; cz <- pCz; symbol ']'; return $ Compr e (fc : cz))
    <|> do e <- between (symbol '[') (symbol ']') pExpz; return $ List e

-- Precedence level low
pOper :: Parser Exp
pOper = pOper' `chainl1` pAddOp

-- Precedence level medium
pOper' :: Parser Exp
pOper' = pExp'' `chainl1` pMulOp

-- ForClause
pForC :: Parser CClause
pForC = lexeme $ do try (string "for"); notFollowedBy (satisfy isBoaAlphaNum); spaces; i <- pIdent; spaces; string "in"; notFollowedBy (satisfy isBoaAlphaNum); spaces; CCFor i <$> pExp

-- IfClause
pIfC :: Parser CClause
pIfC = lexeme $ do try (string "if"); notFollowedBy (satisfy isBoaAlphaNum); spaces; CCIf <$> pExp

-- Clausez
pCz :: Parser [CClause]
pCz =
  lexeme $
    do f <- pForC; cz <- pCz; return (f : cz)
      <|> do i <- pIfC; cz <- pCz; return (i : cz)
      <|> return []

pExpz :: Parser [Exp]
pExpz = lexeme $ do pExp `sepBy` symbol ','

-- Not called anywhere in the code. Defined in the AST, which is why we kept it here as a commet.
-- pExps :: Parser [Exp]
-- pExps = lexeme $ do pExp `sepBy1` symbol ','

pIdent :: Parser String
pIdent =
  do
    c <- satisfy isIdentPrefixChar
    cs <- many (satisfy isIdentChar)
    let i = c : cs
    if i `notElem` reservedIdents
      then return i
      else fail "variable can't be a reserved word"

numSign :: Parser Int
numSign =
  do satisfy (== '-'); return (-1)
    <|> return 1

pNum :: Parser Exp
pNum =
  lexeme $ do
    s <- numSign
    ds <- many1 digit
    case ds of
      [] -> fail ""
      [d] -> return $ Const (IntVal (read [d] * s))
      (d : ds') ->
        if d == '0'
          then fail "num constants cant start with 0"
          else return $ Const (IntVal (read (d : ds') * s))

pStr :: Parser String
pStr =
  try (do symbol' '\''; symbol '\''; return []) -- Checks for the empty string
    <|> try
      ( do
          symbol' '\''
          a <- concat <$> many1 escaped -- many1 makes sure to not return [] one chars such as \n \t
          if null a
            then do symbol' '\''; fail "string may only hold ASCII printable charecters"
            else do symbol' '\''; return a
      )
  where
    escaped =
      try (do string "\\\\"; return "\\")
        <|> try (do string "\\\'"; return "\'")
        <|> try (do string "\\\n"; return "")
        <|> try (do string "\\n"; return "\n")
        <|> do
          x <- noneOf ['\'', '\\', '\n']
          if isStrChar x
            then return [x]
            else fail "string may only hold ASCII printable charecters"

pNoneTrueFalse :: Parser Value
pNoneTrueFalse =
  lexeme $
    try (do string "None"; return NoneVal)
      <|> try (do string "True"; return TrueVal)
      <|> try (do string "False"; return FalseVal)

-- Sub parsers

symbol :: Char -> Parser ()
symbol s = lexeme $ do satisfy (s ==); return ()

symbol' :: Char -> Parser ()
symbol' s = do satisfy (s ==); return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; spaces; return a

pComments' :: Parser ()
pComments' =
  try (do spaces; symbol' '#'; manyTill (satisfy isAscii) (char '\n'); return ())
    <|> try (do spaces; symbol' '#'; manyTill (satisfy isAscii) eof; return ()) -- ! Try could properly be removed

pComments :: Parser a -> Parser a
pComments p = do skipMany pComments'; a <- p; skipMany pComments'; return a

pRelOp :: Parser (Exp -> Exp -> Exp)
pRelOp =
  lexeme $
    do try (string "=="); return $ Oper Eq
      <|> do try (string "in"); notFollowedBy (satisfy isBoaAlphaNum); return $ Oper In
      <|> do Oper Less <$ symbol '<'
      <|> do Oper Greater <$ symbol '>'

pRelNegOp :: Parser (Exp -> Exp -> Exp)
pRelNegOp =
  lexeme $
    do try (string "!="); return $ Oper Eq
      <|> try (do string "not"; many1 $ satisfy isSpace; string "in"; return $ Oper In)
      <|> do try (string "<="); return $ Oper Greater
      <|> do try (string ">="); return $ Oper Less

pAddOp :: Parser (Exp -> Exp -> Exp)
pAddOp =
  lexeme $
    do symbol '+'; notFollowedBy isRel; return $ Oper Plus
      <|> do symbol '-'; notFollowedBy isRel; return $ Oper Minus

pMulOp :: Parser (Exp -> Exp -> Exp)
pMulOp =
  lexeme $
    do symbol '*'; notFollowedBy isRel; return $ Oper Times
      <|> try (do string "//"; notFollowedBy isRel; return $ Oper Div)
      <|> do symbol '%'; notFollowedBy isRel; return $ Oper Mod

isRel :: Parser ()
isRel =
  lexeme $
    do symbol '<'; return ()
      <|> do symbol '>'; return ()
      <|> try (do string "=="; return ())
      <|> try (do string "in"; return ())
      <|> try (do string "<="; return ())
      <|> try (do string ">="; return ())
      <|> try (do string "!="; return ())
      <|> try (do string "not"; spaces; string "in"; return ())
      <|> try (do string "not"; return ())

-- Satisfy helpers

isIdentPrefixChar :: Char -> Bool
isIdentPrefixChar c = isBoaAlpha c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isBoaAlphaNum c || c == '_'

isBoaAlpha :: Char -> Bool
isBoaAlpha c =
  let c' = ord c in 
      -- ord c [65,...,90] = [A-Z], ord c [97,..,122] = [a,..,z]
      (c' >= 65 && c' <= 90) || (c' >= 97 && c' <= 122)

isBoaAlphaNum :: Char -> Bool
isBoaAlphaNum c = isDigit c || isBoaAlpha c

isStrChar :: Char -> Bool
isStrChar c = isAscii c && isPrint c

-- Constants

reservedIdents :: [String]
reservedIdents = ["None", "True", "False", "for", "if", "in", "not"]