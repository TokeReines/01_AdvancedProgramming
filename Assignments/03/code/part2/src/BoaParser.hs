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

pStmts :: Parser [Stmt] -- or Parser Program. ! Unsure about the order of these
pStmts =
  do stmt <- pStmt; return [stmt]
    <|> do stmt <- pStmt; symbol ';'; stmts <- pStmts; return (stmt : stmts)

pStmt :: Parser Stmt
pStmt =
  try (do i <- pIdent; symbol '='; SDef i <$> pExp)
    <|> do SExp <$> pExp


pExp :: Parser Exp
pExp = lexeme $
         try (do e1 <- pExp'; ro <- pRelNegOp; e2 <- pExp'; return $ Not (ro e1 e2))
         <|> try (do e1 <- pExp'; ro <- pRelOp; e2 <- pExp'; return $ ro e1 e2)
         <|> pExp'

pExp' :: Parser Exp 
pExp' = do pOper;
        <|> do pExp''

pExp'' :: Parser Exp
pExp'' =
  do symbol '['; e <- pExp; fc <- pForC; cz <- pCz;  symbol ']'; return $ Compr e (fc:cz);
  <|> try (do pNum) 
  <|> try (do Const . StringVal <$> pStr) 
  <|> try (do Const <$> pNoneTrueFalse) 
  <|> try (do i <- pIdent; spaces; symbol '('; ez <- pExpz; symbol ')'; return $ Call i ez;)
  <|> try (do Var <$> pIdent) 
  <|> do lexeme $ string "not"; Not <$> pExp
  <|> try (do between (symbol '(') (symbol ')') pExp)
  <|> try (do e <- between (symbol '[') (symbol ']') pExpz; return $ List e;)
  

-- Precedence level low
pOper = pOper' `chainl1` pAddOp
-- Precedence level medium
pOper' = pOper'' `chainl1` pMulOp
pOper'' = pExp''

pRelOp :: Parser (Exp -> Exp -> Exp)
pRelOp = lexeme $ try (do string "=="; return $ Oper Eq)
          <|> try (do string "in"; return $ Oper In)
          <|> do Oper Less <$ symbol '<'
          <|> do Oper Greater <$ symbol '>'

pRelNegOp :: Parser (Exp -> Exp -> Exp)
pRelNegOp = lexeme $ try (do string "!="; return $ Oper Eq)
          <|> try (do string "not"; spaces; string "in"; return $ Oper In)
          <|> try (do string "<="; return $ Oper Greater)
          <|> try (do string ">="; return $ Oper Less)


pAddOp :: Parser (Exp -> Exp -> Exp) -- Use chain1l for this
pAddOp =
    do Oper Plus <$ symbol '+'
    <|> do Oper Minus <$ symbol '-'

pMulOp :: Parser (Exp -> Exp -> Exp) -- Use chain1l for this
pMulOp =
      do Oper Times <$ symbol '*'
      <|> try (do string "//"; return $ Oper Div)
      <|> do Oper Mod <$ symbol '%'

-- ForClause
pForC :: Parser CClause
pForC = lexeme $ try (do string "for"; spaces; i <- pIdent; try (string "in"); spaces; e <- pExp; return $ CCFor i e)

-- IfClause
pIfC :: Parser CClause
pIfC = lexeme $ do try (string "if"); spaces; CCIf <$> pExp;

-- Clausez 
pCz :: Parser [CClause]
pCz = lexeme $ do f <- pForC; cz <- pCz; return (f : cz);
      <|> do i <- pIfC; cz <- pCz; return (i : cz);
      <|> return []

pExpz :: Parser [Exp]
pExpz = lexeme $ do pExp `sepBy` symbol ',';

-- Not called anywhere in the code. Defined in the AST, which is why we kept it here.
pExps :: Parser [Exp]
pExps = lexeme $ do pExp `sepBy1` symbol ',';

pIdent :: Parser String
pIdent = lexeme $ try (do
  c <- satisfy isIdentPrefixChar
  cs <- many (satisfy isIdentChar)
  let i = c : cs
  if i `notElem` reservedIdents
    then return i
    else fail "variable can't be a reserved word")

pNum :: Parser Exp
pNum =
  lexeme $ do
    s <- numSign
    ds <- many1 digit
    case ds of
      [] -> fail ""
      [d] -> return $ Const (IntVal (digitToInt d * s))
      (d : ds') ->
        if d == '0'
          then fail "num constants cant start with 0"
           else return $ Const (IntVal (read (d : ds') * s))

pStr :: Parser String
pStr = do
  symbol '\''
  a <- concat <$> many escaped
  symbol '\''
  return a
  where
    escaped =
      try (do string "\\\\"; return ['\\'])
        <|> try (do string "\\\'"; return ['\''])
        <|> try (do string "\\\n"; return [])
        <|> try (do string "\n"; return ['\n'])
        -- TODO Consider/ask what specific chars are allowed
        <|> try
          ( do
              x <- noneOf ['\'', '\\']
              if isStrChar x
                then return [x]
                else fail "string may only hold ASCII printable charecters"
          )

pNoneTrueFalse :: Parser Value
pNoneTrueFalse =
  lexeme $
    try (do string "None"; return NoneVal)
      <|> try (do string "True"; return TrueVal)
      <|> try (do string "False"; return FalseVal)

-- pNot :: Parser Value 
-- pNot =
--   lexeme $
--     try ()

-- Sub parsers

symbol :: Char -> Parser ()
-- symbol s = lexeme $ do satisfy (s ==) <?> [s]; return ()
symbol s = lexeme $ do satisfy (s ==); return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; spaces; return a

isRel :: Parser ()
isRel = lexeme $
    do symbol '<'; return ()
    <|> do symbol '>'; return ()
    <|> try (do string "=="; return ())
    <|> try (do string "in"; return ())
    <|> try (do string "<="; return ())
    <|> try (do string ">="; return ())
    <|> try (do string "!="; return ())
    <|> try (do string "not"; spaces; string "in"; return ())

-- Helper functions

numSign :: Parser Int
numSign =
  do symbol '-'; return (-1)
    <|> return 1

-- Satisfy helpers


isIdentPrefixChar :: Char -> Bool
isIdentPrefixChar c = isBoaAlpha c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isBoaAlphaNum c || c == '_'

isBoaAlpha :: Char -> Bool
isBoaAlpha c =
  let c' = ord c in
    -- ord [65...90] = [A...Z] and [97...122] = [a...z]
    (c' >= 65 && c' <= 90 ) || (c' >= 97 && c' <= 122 )

isBoaAlphaNum :: Char -> Bool
isBoaAlphaNum c = isDigit c  || isBoaAlpha c

isStrChar :: Char -> Bool
isStrChar c = isAscii c && isPrint c

-- Constants

reservedIdents :: [String]
reservedIdents = ["None", "True", "False", "for", "if", "in", "not"]