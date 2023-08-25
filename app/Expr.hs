module Expr where

import Parser

data Expr = Con Int | Bin Op Expr Expr
data Op   = Plus | Minus

expr :: Parser Expr
expr = token (constant <|> paren binary)
constant = do {n <- nat;return (Con n)}
binary = do {e1 <- expr;p <- op;e2 <- expr;return (Bin p e1 e2)}
op = (symbol "+" >> return Plus) <|> 
     (symbol "-" >> return Minus)
paren p = do {symbol "(";e <- p;symbol ")";return e}