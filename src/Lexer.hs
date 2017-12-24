{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Lexer where

import Data.Void
import Data.Functor.Identity
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

integer :: Parser Integer
integer = lexeme L.decimal

double :: Parser Double
double = lexeme L.float

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["->"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

binary :: String -> (a -> a -> a) -> Operator (ParsecT Void String Identity) a
binary  name f = InfixL  (f <$ symbol name)

prefix :: String -> (a -> a) -> Operator (ParsecT Void String Identity) a
prefix  name f = Prefix  (f <$ symbol name)

postfix :: String -> (a -> a) -> Operator (ParsecT Void String Identity) a
postfix name f = Postfix (f <$ symbol name)
