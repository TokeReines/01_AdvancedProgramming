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
  -- do i <- pIdent; symbol '='; e <- pExp; return (SDef i e)
  do i <- pIdent; symbol '='; SDef i <$> pExp'
    <|> do SExp <$> pExp' -- do e <- pExp; return (SExp e)

pExp' :: Parser Exp -- Called before Exp
pExp' = do pOper1;
        -- ! Add relational parser here
        <|> do pExp

pExp :: Parser Exp
pExp =
  do Const . IntVal <$> pNum -- n <- pNum; return (Const (IntVal n))
    <|> do Const . StringVal <$> pStr -- s <- pStr; return (Const (StringVal s))
    <|> do Const <$> pNoneTrueFalse -- v <- pNoneTrueFalse; return $ Const v
    <|> do Var <$> pIdent -- do i <- pIdent; return $ Var i --! Needs tests do
    <|> try (do string "not"; notFollowedBy (satisfy isBoaAlphaNum); spaces; Not <$> pExp')
    <|> do symbol '('; e <- pExp'; symbol ')'; return e; -- ! Move this to helper such that Oper can use it in chainl1
    <|> do i <- pIdent; symbol '('; ez <- pExpz; symbol ')'; return $ Call i  ez;
    <|> do symbol '['; ez <- pExpz; symbol ']'; return $ List ez;
    <|> do symbol '['; e <- pExp'; fc <- pForC; cz <- pCz;  symbol ']'; return $ Compr e (fc:cz);

pOper :: Parser Op -- (Op -> Exp -> Exp) -- Use chain1l for this
pOper =
  lexeme $
    do Plus <$ symbol '+'
      <|> do Minus <$ symbol '-'
      <|> do Times <$ symbol '*'
      <|> do Div <$ string "//" -- ! Might need a try
      <|> do Mod <$ symbol '%'
      <|> do Eq <$ string "==" -- ! Might need a try
      -- <|> do string "!="; return (Not Eq) -- ! Might need a try
      <|> do Less <$ symbol '<'
      -- <|> do string "<="; return ? -- ! Might need a try
      <|> do Greater <$ symbol '>'

-- <|> do string ">="; return ?
-- <|> do string "in" return ?
-- <|> do string "not"; spaces; string "in" return ?

-- Precedence level low
-- pOper0 :: Parser Exp
-- pOper0 = pOper1 `chainl1` pRelOp;

-- ! This kind of works but as thy are non-associative it might just work to handle it in in pExp' and setting it before or after pOper depending one how the precedens is called
-- pOper0' :: Parser Exp
-- pOper0' = do a <- pOper1 `chainl1` pRelOp'; return (Not a);
-- Precedence level medium
pOper1 :: Parser Exp
pOper1 = pOper2 `chainl1` pAddOp
-- Precedence level high
pOper2 :: Parser Exp
pOper2 = pExp `chainl1` pMulOp

-- pRelOp :: Parser (Exp -> Exp -> Exp) -- Use chain1l for this
-- pRelOp = lexeme $
--     do Oper Less <$ symbol '<'
--     <|> do Oper Greater <$ symbol '>'
--     <|> do Oper Eq <$ string "=="
--     <|> do Oper In <$ string "in"

-- pRelOp' :: Parser (Exp -> Exp -> Exp) -- Use chain1l for this
-- pRelOp' = lexeme $
--     do Oper Greater <$ string "<="
--     <|> do Oper Less <$ string ">="
--     <|> do Oper Div <$ string "!="
--     <|> do Oper In <$ string "in"
--     -- <|> do string "not"; spaces; string "in"; Oper In
    

pAddOp :: Parser (Exp -> Exp -> Exp) -- Use chain1l for this
pAddOp =
    do Oper Plus <$ symbol '+'
    <|> do Oper Minus <$ symbol '-'

pMulOp :: Parser (Exp -> Exp -> Exp) -- Use chain1l for this
pMulOp =
      do Oper Times <$ symbol '*'
      <|> do Oper Div <$ string "//" -- ! Might need a try
      <|> do Oper Mod <$ symbol '%'

-- ForClause
pForC :: Parser CClause
pForC = undefined

-- IfClause
pIfC :: Parser Exp
pIfC = undefined

-- Clausez 
pCz :: Parser [CClause]
pCz = undefined

pCCf :: Parser (String -> Exp)
pCCf = undefined

pExpz :: Parser [Exp]
pExpz = undefined

pExps :: Parser Exp
pExps = undefined

pIdent :: Parser String -- Or VName or FName maybe in an either Monad?
pIdent = lexeme $ try (do
  c <- satisfy isIdentPrefixChar
  cs <- many (satisfy isIdentChar)
  let i = c : cs
  if i `notElem` reservedIdents
    then return i
    else fail "variable can't be a reserved word")

pNum :: Parser Int
pNum =
  lexeme $ do
    s <- numSign
    ds <- many1 digit
    case ds of
      [] -> fail ""
      [d] -> return $ digitToInt d * s
      (d : ds') ->
        if d == '0'
          then fail "num constants cant start with 0"
          else return $ read (d : ds') * s

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
    try (do string "None"; return TrueVal)
      <|> try (do string "True"; return TrueVal)
      <|> try (do string "False"; return TrueVal)


-- Sub parsers

symbol :: Char -> Parser ()
symbol s = lexeme $ do satisfy (s ==) <?> [s]; return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; spaces; return a


-- Helper functions

numSign :: Parser Int
numSign =
  do c <- symbol '-'; return (-1)
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