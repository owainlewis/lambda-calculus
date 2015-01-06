module Language.Combinators
  ( sCombinator
  , kCombinator
  , iCombinator
  , yCombinator
  ) where

-- SKI Combinator Logic

sCombinator :: (a -> b -> c) -> (a -> b) -> a -> c
sCombinator f g x = f x (g x)

kCombinator :: a -> b -> b
kCombinator x y = y

k1 :: a -> b -> b
k1 x y = y

iCombinator :: a -> a
iCombinator x = x

-- The fixed point Y combinator
yCombinator :: ((a -> a) -> (a -> a)) -> (a -> a)
yCombinator f = f (y f)

compose :: (s -> t) -> (t1 -> s) -> t1 -> t
compose f g x = f $ g x

twice :: (a -> a) -> a -> a
twice f = f . f
