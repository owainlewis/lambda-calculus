{-# LANGUAGE FlexibleContexts #-}
module Language.LambdaCalculus
  ( Term(..)
  , parseTerm
  ) where

import           Control.Applicative ((*>), (<*))
import           Text.Parsec
import           Text.Parsec.String

type Identifier = String

-- <expresson> ::= <name> | <function> | <application>

data Term = Var String
          | Function String Term
          | Application Term Term
            deriving ( Show, Eq )

lexeme :: Stream s m Char =>
          ParsecT s u m a ->
          ParsecT s u m a
lexeme p = spaces *> p <* spaces

lambdaExprParser :: Parser Term
lambdaExprParser = do char '\\'
                      var <- many1 letter
                      lexeme $ char '.'
                      expr <- lambdaExprParser
                      return $ Function var expr
               <|> do apps <- many1 term
                      return $ foldl1 Application apps

parseVar :: Parser Term
parseVar = many1 letter >>= (\v -> return $ Var v)

parseFunction :: Parser Term
parseFunction = do
    char '('
    expr <- lambdaExprParser
    char ')'
    return expr

term :: Parser Term
term = parseFunction

parseTerm :: String -> Either ParseError Term
parseTerm input = parse term " " input
