module ParserImpl where

import Absyn

import Text.ParserCombinators.ReadP
    ( ReadP,
      between,
      chainl1,
      chainr1,
      char,
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
import Data.Char (toLower, isAlpha, isAscii, isDigit)
import Text.Parsec.Prim (parserBind)

type Parser a = ReadP a -- Use ReadP

-- Check that remaining input does not start with a character satisfying p
notFollowedByAny :: (Char -> Bool) -> Parser ()
notFollowedByAny p =
    do s <- munch p; if s == "" then return () else pfail

-- Skip over a complete comment
comment :: Parser ()
comment =
    between (char '{') (char '}') $
        skipMany $ do satisfy(`notElem` "{}"); return ()
                   <|> comment

-- Skip over whitespace and/or comments
whitespace :: Parser ()
whitespace = skipMany $ do satisfy (`elem` " \n\t"); return ()
                        <|> comment

-- Parse a single token, skipping over trailing ws
lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

-- Parse a specific symbol
symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

-- parse (case-insensitively) a specific keyword, itself in lowercase
keywordCI :: String -> Parser ()
keywordCI s = lexeme $
    do mapM_ (\c -> satisfy (\c' -> toLower c' == c)) s
       notFollowedByAny (\c -> isLetter c || isDigit c || c == '-')

-- Check whether c is an ASCII letter (a-z, A-Z)
isLetter :: Char -> Bool
isLetter c = isAscii c && isAlpha c

parseString :: String -> Either String IDB
parseString s = 
    case readP_to_S  (between whitespace eof pDatabase) s of
        [] -> Left "no parse"
        [(idb, _)] -> Right idb
        _ -> error "ambiguous grammar"

-- Parsers for specific grammar symbols
-- Nonterminals

-- Start symbol of the grammar
pDatabase :: Parser IDB
pDatabase = do ds <- many (pResDecl <|> pCompDecl)
               return $ foldl (<>) mempty ds -- ([a], [b]) instance of Monoid

pResDecl :: Parser IDB
pResDecl =
    do keywordCI "resource"
       rs <- pRName `sepBy1` (symbol ",")
       symbol "."
       return (rs, [])
       
pCompDecl :: Parser IDB
pCompDecl =
    do keywordCI "component"
       n <- pCName
       symbol ":"
       cs <- pClause `sepBy1` (symbol ";")
       symbol "."
       return ([], [IC n cs])

pClause :: Parser (CKind, RSpec)
pClause =
    choice [do keywordCI "provides"; s <- pRSpec; return (CKProvides, s),
            do keywordCI "uses"; s <- pRSpec; return (CKUses , s),
            do keywordCI "requires"; s <- pRSpec; return (CKRequires , s)]

pRSpec :: Parser RSpec
pRSpec = pRSpecDisj

pRSpecDisj :: Parser RSpec
pRSpecDisj = pRSpecConj `chainl1` (do symbol "|"; return RSOr)

pRSpecConj :: Parser RSpec
pRSpecConj = pRSpecScale `chainl1` (do symbol ","; return RSAnd)

pRSpecScale :: Parser RSpec
pRSpecScale = pRSpecAtomic
              <|> do n <- pNum; r <- pRSpecScale; return $ RSNum n r

pRSpecAtomic :: Parser RSpec
pRSpecAtomic = do i <- pRName; return $ RSRes i

-- Terminals
pRName :: Parser RName
pRName = pName

pCName :: Parser CName
pCName = pName

pName :: Parser String
pName = lexeme $
    do s <- pWord `chainr1` (do char '-'; return $ \s1 s2 -> s1 ++ "-" ++ s2)
       if length s <= 32 then return s else fail $ "name too long: " ++ s
    where
        pWord :: Parser String
        pWord = do s <- munch1 (\c -> isDigit c || isLetter c)
                   if any isLetter s then return s
                   else fail $ "no letter in word: " ++ s

pNum :: Parser Int
pNum = lexeme $
    do ds <- munch1 isDigit
       notFollowedByAny (\c -> isLetter c || c == '-')
       let n = read ds -- won't fail, because ds contains only digits
       if n <= 999999 then return n else fail $ "number too big: " ++ show n