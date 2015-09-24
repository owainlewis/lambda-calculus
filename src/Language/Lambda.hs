{-# LANGUAGE OverloadedStrings #-}

-- A parser for Lambda Calculus
module Lambda
  ( Expr(..)
  , Lit(..)
  ) where

import           Data.Char
import           Text.Parsec
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Expr     as Ex
import qualified Text.Parsec.Token    as Tok

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit
  deriving ( Eq, Show )

data Lit
  = LInt Int
  | LBool Bool
  deriving ( Eq, Show )

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops   = ["λ"]
        names = []
        style = emptyDef {Tok.reservedOpNames = ops,
                          Tok.reservedNames   = names,
                          Tok.commentLine     = "#"}

data Lambda = Lambda

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

natural :: Parser Integer
natural = Tok.natural lexer

variable :: Parser Expr
variable = identifier >>= (return . Var)

number :: Parser Expr
number = natural >>= return . (\x -> (Lit (LInt (fromIntegral x))))

lambda :: Parser Expr
lambda = do
  reservedOp "λ"
  args <- many1 identifier
  reservedOp "."
  body <- expr
  return $ foldr Lam body args

term :: Parser Expr
term =  parens expr
    <|> lambda
    <|> variable
    <|> number

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)

-- Utility function for testing a single parser
doParse :: Parser a -> String -> Either ParseError a
doParse p input = parse p "=>" input

-- parseExpr "(λx.x)"
parseExpr :: String -> Either ParseError Expr
parseExpr input = parse (contents expr) "=>" input
