{-# LANGUAGE OverloadedStrings #-}
module Language.Parser
  ( AST(..)
  , isWhiteSpace
  , lexer) where
  
import           Data.Char

type Token = String

data AST =
    C Char
  | Num Integer
  | Var String

lexer (x:xs)
  | isSpace x = lexer xs
  | isDigit x = let num = x : takeWhile isDigit xs
                    ys = dropWhile isDigit xs
                in num : lexer ys
  | otherwise = [x] : lexer xs
lexer [] = []

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlpha c || isDigit c || (c == '_')

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

type Parser a = [Token] -> [(a, [Token])]

parseLiteral :: String -> Parser String
parseLiteral s (t:ts) = if s == t then [(s, ts)] else []
parseLiteral _ [] = []
