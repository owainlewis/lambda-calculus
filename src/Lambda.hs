module Lambda ( ) where

data Expression = Literal String
                | Lambda String Expression
                | Apply Expression Expression
                deriving Show

type Context = [(String, Expression)]
