module Parser where

import Control.Applicative
import Data.Void
import Data.Functor.Identity
import Text.Megaparsec
import Text.Megaparsec.Expr

import Syntax
import Lexer

number :: Parser Expr
number = do
  n <- try double
  return (Lit n)

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

table :: [[Operator Parser Expr]]
table =
  [ [binary "*" (BinOp Mul), binary "/" (BinOp Div)]
  , [binary "+" (BinOp Add), binary "-" (BinOp Sub)]
  ]

function :: Parser Expr
function = do
  name <- identifier
  symbol "="
  try (fnWithArgs name) <|> try (fnNoArgs name)
    where
      fnWithArgs name = do
        args <- commaSep identifier
        symbol "->"
        body <- expr
        return $ Function name args body
      fnNoArgs name  = do
        body <- expr
        return $ Function name [] body

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

contents :: Parser a -> Parser a
contents p = do
  sc
  r <- p
  eof
  return r

term :: Parser Expr
term = try call
   <|> try variable
   <|> try number
   <|> parens expr

topLevel :: Parser Expr
topLevel = try function
       <|> try call
       <|> expr

expr :: Parser Expr
expr = makeExprParser term table

parseExpr :: String -> Either (ParseError Char Void) Expr
parseExpr = runParser (contents expr) "<stdin>"

parseTopLevel :: String -> Either (ParseError Char Void) Expr
parseTopLevel = runParser (contents topLevel) "<stdin>"
