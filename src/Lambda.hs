{-# LANGUAGE FlexibleContexts #-}
module Lambda
  ( Term(..)
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
term = parseVar <|> parseFunction

identityFunction = "\\x.x"
test input = parseTest (do expr <- term; eof; return expr) input

-- x = "\\y.y(\\x.x)y"

-- SKI Combinator Logic

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

k :: a -> b -> b
k x y = y

k1 :: a -> b -> b
k1 x y = y

i :: a -> a
i x = x

-- The fixed point Y combinator
y :: ((a -> a) -> (a -> a)) -> (a -> a)
y f = f (y f)

compose :: (s -> t) -> (t1 -> s) -> t1 -> t
compose f g x = f $ g x

twice :: (a -> a) -> a -> a
twice f = f . f
