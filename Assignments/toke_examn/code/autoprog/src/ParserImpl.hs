-- Put your Parser implementation in this file
module ParserImpl where

import Defs
-- import either ReadP or Parsec, as relevant
-- import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadP
    ( ReadP,
      between,
      chainl1,
      chainr1,
      choice,
      eof,
      many,
      munch,
      munch1,
      pfail,
      readP_to_S,
      satisfy,
      sepBy1,
      skipMany,
      string )
import Control.Applicative ((<|>))
import Data.Char (toLower, isAlpha, isAscii, isDigit, isUpper, isSymbol, isLetter, isLower)
import Data.List

-- Type ::= TVar
    -- | Type2 ‘->’ Type2
    -- | ‘(’ Type ‘,’ Type ‘)’
    -- | ‘(’ Type ‘)’

-- Type2 ::= TCon Typez
    -- | Type 

-- Typez ::= 𝜖
    -- | Type Typez

type Parser a = ReadP a -- Use ReadP

parseStringType2 :: String -> [(PType, String)]
parseStringType2 s = readP_to_S (between whitespace eof pType) s

parseStringType :: String -> EM PType
parseStringType s =
    case readP_to_S (between whitespace eof pType) s of
        [] -> Left "no parse"
        [(ptype, _)] -> Right ptype
        _ -> error "ambiguous grammar"

-- TDeclz ::=𝜖
-- | TDeclz TDecl
-- | TDeclz ‘;’
-- TDeclz ::= 𝜖
-- | TDecl
-- | TDeclz ‘;’

parseStringTDeclz :: String -> EM [TDecl]
parseStringTDeclz s =
    case readP_to_S (between whitespace eof pTDeclz) s of
        [] -> Left "no parse"
        [(ptype, _)] -> Right ptype
        _ -> error "ambiguous grammar"

pTDeclz :: Parser [TDecl]
pTDeclz = many pTDecl

-- Note: All these decl parsers could be written into a single parser function with keyword and structure as arguments
pTDecl :: Parser TDecl
pTDecl = pTDeclType
    <|> pTDeclNewType
    <|> pTDeclData


pTDeclType :: Parser TDecl
pTDeclType =
    do symbol "type"
       h <- pTDHead
       symbol "="
       t <- pType
       return $ TDSyn h (t)

--TDRcd TDHead RCName [(FName, PType)]
pTDeclNewType :: Parser TDecl
pTDeclNewType =
    do symbol "newtype"
       h <- pTDHead
       symbol "="
       r <- pRCon
       symbol "{"
       f <- pField
       symbol "::"
       t <- pType
       symbol "}"
       return $ TDRcd h r [(f, t)]

pTDeclData :: Parser TDecl
pTDeclData =
    do symbol "data"
       h <- pTDHead
       symbol "="
       r <- pRCon
       symbol "{"
       d <- pFDeclz
       symbol "}"
       return $ TDRcd h r d

pFDeclz :: Parser [(FName, PType)]
pFDeclz = do d <- pFDecl `sepBy1` symbol ","; return $ concat d

pFDecl :: Parser [(FName, PType)]
pFDecl =
    do fs <- pFields
       symbol "::"
       t <- pType
       return $ map (\f -> (f, t)) fs

pFields :: Parser [FName]
pFields = do pField `sepBy1` symbol ",";

pTDHead :: Parser (TCName, [TVName])
pTDHead = do c <- pTCon; t <- pTVarz; return $ (c, t)

pTVarz :: Parser [TVName]
pTVarz = many pTVar

pType :: Parser PType
pType = do pTApp

pTApp :: Parser PType
pTApp = do pTTCon `chainr1` (do symbol "->"; return $ \r1 r2 -> PTApp "(->)" [r1, r2]);

pTTCon :: Parser PType
pTTCon = do
    c <- pTCon
    PTApp c <$> pTypez
    <|> pTTuple

pTypez :: Parser [PType]
pTypez = many $
    do c <- pTCon
       return $ PTApp c []
    <|> pTTuple

pTTuple :: Parser PType
pTTuple = do
    symbol "("
    l <- pType
    symbol ","
    r <- pType
    symbol ")"
    return $ PTApp "(,)" [l, r]
    <|> pTPType

pTPType :: Parser PType
pTPType = do between (symbol "(") (symbol ")") pType
    <|> pTTVar

pTTVar :: Parser PType
pTTVar = do PTVar <$> pTVar;

-- Auxiliary functions stolen from examn 2019
-- Parse a specific symbol
symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

-- Precedence level low
comment :: Parser ()
comment =
    between (symbol "{-") (symbol "-}") $
        skipMany $ do satisfy (\c -> isAlpha c || isDigit c); return ()
    <|> comment

-- Skip over whitespace and/or comments
whitespace :: Parser ()
whitespace = skipMany $ do satisfy (`elem` " \n\t"); return ()
    <|> comment

-- Parse a single token, skipping over trailing ws
lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a


-- Terminals

pTCon :: Parser TCName
pTCon = pCName

pTVar :: Parser TVName
pTVar = pVName

pRCon :: Parser RCName
pRCon = pCName

pField :: Parser FName
pField = pVName

-- Parse a constructor name - ASCII, digits, apostrophes and underscores, must start with UPPERCASE letter
pCName :: Parser String
pCName = lexeme $
    do c <- satisfy isUpper
       cs <- pName
       let i = c:cs
       if i `notElem` reservedNames
           then return i
       else error "variable can't be a reserved word"

-- Parse a varialbe name - ASCII, digits, apostrophes and underscores, must start with LOWERCASE letter
pVName :: Parser String
pVName = lexeme $
    do c <- satisfy isLower
       cs <- pName
       let i = c:cs
       if i `notElem` reservedNames
           then return i
       else error "variable can't be a reserved word"

pName :: Parser String
pName = munch (\c -> isDigit c || isLetter c || isSymbol '\'')

reservedNames :: [String]
reservedNames = ["type", "newtype", "data"]