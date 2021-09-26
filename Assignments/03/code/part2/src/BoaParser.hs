-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
-- add any other other imports you need

import Data.Char
import Text.ParserCombinators.Parsec

parseString :: String -> Either ParseError Program
parseString = parse (do spaces; a <- pProgram; eof; return a) ""

pProgram :: Parser Program
pProgram = do pStmts

pStmts :: Parser [Stmt] -- or Parser Program. ! Unsure about the order of these
pStmts =
  do stmt <- pStmt; return [stmt]
    <|> do stmt <- pStmt; symbol ';'; stmts <- pStmts; return (stmt : stmts)

pStmt :: Parser Stmt
pStmt =
  -- do i <- pIdent; symbol '='; e <- pExp; return (SDef i e)
  do i <- pIdent; symbol '='; SDef i <$> pExp
    <|> do SExp <$> pExp -- do e <- pExp; return (SExp e)

pExp :: Parser Exp
pExp =
  do Const . IntVal <$> pNum -- n <- pNum; return (Const (IntVal n))
    <|> do Const . StringVal <$> pStr;  -- s <- pStr; return (Const (StringVal s))
    <|> do Const <$> pNoneTrueFalse;  -- v <- pNoneTrueFalse; return $ Const v
    <|> do Var <$> pIdent; -- do i <- pIdent; return $ Var i --! Needs tests do 
    --  chainl1 pExp pOper-- e1 <- pExp; o <-pOper; e2 <- pExp; return e1 e2  -- e <- pExp; oper exp
    -- <|> not Expr
    -- <|> do symbol '('; e <- pExp; symbol ')'; return e
    -- <|> do i <- pIden; symbol '('; e <- pE; symbol ')'; return e
    -- <|> [ Exprz ]
    -- <|> [ Expr ForClause Clausez ]
    

-- <|> do i <- pIdent; return (VName x)

pOper :: Parser Op -- (Op -> Exp -> Exp) -- Use chain1l for this
pOper =
  lexeme $
      do Plus <$ symbol '+';
      <|> do Minus <$ symbol '-'; 
      <|> do Times <$ symbol '*';
      <|> do Div <$ string "//"; 
      <|> do Mod <$ symbol '%'; 
      <|> do Eq <$ string "==";
      -- <|> do string "!="; return (Not Eq)
      <|> do Less <$  symbol '<';
      -- <|> do string "<="; return ?
      <|> do Greater <$ symbol '>'; 
      -- <|> do string ">="; return ?
      -- <|> do string "in" return ?
      -- <|> do string "not"; spaces; string "in" return ?

pOperLA = undefined
pOperRA = undefined

-- ForClause
pForC :: Parser CClause
pForC = undefined

-- IfClause
pIfC :: Parser Exp
pIfC = undefined

pCCf :: Parser (String -> Exp)
pCCf = undefined

pIdent :: Parser String -- Or VName or FName maybe in an either Monad?
pIdent = lexeme $ do
  c <- satisfy isIdentPrefixChar
  cs <- many (satisfy isIdentChar)
  let i = c : cs
  if i `notElem` reservedIdents
    then return i
    else fail "variable can't be a reserved word"

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
              else return (read (d : ds') * s)

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

---- Helper functions

symbol :: Char -> Parser ()
symbol s = lexeme $ do satisfy (s ==) <?> [s]; return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; spaces; return a

numSign :: Parser Int
numSign = do c <- symbol '-'; return (-1)
          <|> return 1

-- Satisfy helpers

isIdentPrefixChar :: Char -> Bool
isIdentPrefixChar c = isAlpha c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlpha c || c == '_' || isDigit c

isNumChar :: Char -> Bool
isNumChar c =
  let c' = ord c
   in c' >= 32 && c' <= 126 -- Allow \t ? () || c == 9

isStrChar :: Char -> Bool
isStrChar c = isAscii c && isPrint c


-- Constants

reservedIdents :: [String]
reservedIdents = ["None", "True", "False", "for", "if", "in", "not"]