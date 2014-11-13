module Lambda ( ) where

import           Text.Parsec
import           Text.Parsec.String

type Identifier = String

data LambdaExpr = Variable Char
                | Abstraction Char LambdaExpr
                | Application LambdaExpr LambdaExpr
                deriving Show

lambdaExprParser :: Parser LambdaExpr
lambdaExprParser = do char '\\'
                      var <- letter
                      char '.'
                      expr <- lambdaExprParser
                      return $ Abstraction var expr
               <|> do apps <- many1 term
                      return $ foldl1 Application apps

term :: Parser LambdaExpr
term = do var <- letter
          return $ Variable var
   <|> do char '('
          expr <- lambdaExprParser
          char ')'
          return expr

lC :: Parser LambdaExpr
lC = term <|> lambdaExprParser

--go input = parse (lambdaCalculus input) " "
test input = parseTest (do expr <- lC; eof; return expr) input

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

compose :: (s -> t) -> (t1 -> s) -> t1 -> t
compose f g x = f $ g x

twice :: (a -> a) -> a -> a
twice f = f . f
